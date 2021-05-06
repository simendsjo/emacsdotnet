module Simendsjo.EmacsDotNet.Lisp

open System
open System.Text
open System.Reflection
open Microsoft.FSharp.Reflection

open FParsec

type ExposeToEmacs() =
    inherit Attribute()
    
type ExposeToEmacsAsTopLevelKebabCase() =
    inherit ExposeToEmacs()

// https://www.gnu.org/software/emacs/manual/html_node/elisp/Programming-Types.html
// https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Read-Syntax.html
type SExpr =
    /// nil / '()
    /// Nil means null, empty list, false -- impossible to distinguish them, good call emacs
    | Nil
    /// ?C
    /// A character, C
    | Character of char
    | Symbol of string
    | Integer of int
    | Float of float
    | String of string
    // TODO: Cons not supported yet
    //| Cons of SExpr * SExpr
    | List of SExpr list

// https://stackoverflow.com/a/46026233
let sexprTag = FSharpValue.PreComputeUnionTagReader(typeof<SExpr>)

let (|FunCall|_|) (expr : SExpr) =
    match expr with
    | SExpr.List (Symbol f::xs) ->
        Some (Symbol f, xs)
    | _ ->
        None


module Printer =
    let printExpr (expr : SExpr) : string =
        let rec go (sb : StringBuilder) (expr : SExpr) =
            match expr with
            | Nil -> sb.Append("nil")
            | Symbol x -> sb.Append(x)
            | Integer x -> sb.Append(x)
            | Float x -> sb.Append(x)
            | String x -> sb.Append('"').Append(x).Append('"')
            | Character x -> sb.Append('?').Append(x)
            | SExpr.List [] -> sb.Append("()")
            | SExpr.List (x::xs) ->
                sb.Append('(') |> ignore
                go sb x |> ignore
                for x in xs do
                    sb.Append(' ') |> ignore
                    go sb x |> ignore
                sb.Append(')')
           
        go (StringBuilder()) expr
        |> fun sb -> sb.ToString()


// Naive conversions between casing.
// Enough for my use-case
[<AutoOpen>]
module CaseConvert =
    let toCamelCase (orig : string) : string =
      orig.ToCharArray()
      |> Seq.indexed
      |> Seq.fold (fun (wasBreak, sb : StringBuilder) (i, c) ->
        if i = 0 then
          (false, sb.Append(System.Char.ToUpper(c)))
        elif c = '-' || c = '_' then
          (true, sb)
        elif wasBreak then
          (false, sb.Append(System.Char.ToUpper(c)))
        else
          (false, sb.Append(c))
        ) (false, StringBuilder())
      |> fun (_, sb) -> sb.ToString()
      
    let toPascalCase (orig : string) : string =
      orig.ToCharArray()
      |> Seq.indexed
      |> Seq.fold (fun (wasBreak, sb : StringBuilder) (i, c) ->
        if i = 0 then
          (false, sb.Append(System.Char.ToLower(c)))
        elif c = '-' || c = '_' then
          (true, sb)
        elif wasBreak then
          (false, sb.Append(System.Char.ToUpper(c)))
        else
          (false, sb.Append(c))
        ) (false, StringBuilder())
      |> fun (_, sb) -> sb.ToString()
      
    /// someString -> some-string
    /// SomeString -> some-string
    /// SomeHTMLMethod -> some-h-t-m-l-method (ooops...)
    let toKebabCase (orig : string) : string =
      orig.ToCharArray()
      |> Seq.indexed
      |> Seq.fold (fun (sb : StringBuilder) (i, c) ->
        if i = 0 then
          sb.Append(System.Char.ToLower(c))
        elif System.Char.IsUpper(c) then
          sb.Append('-').Append(System.Char.ToLower(c))
        elif c = '_' then
          sb.Append('-')
        else
          sb.Append(c)
        ) (StringBuilder())
      |> fun sb -> sb.ToString()


module Parser =
    let parseNil =
        pstring "nil"
        |>> fun _ -> Nil
        .>> spaces
        
    let parseSymbol =
        many1Chars (anyOf "+-*/_~!@$%^&=:<>{}" <|> letter <|> digit)
        |>> Symbol
        .>> spaces
        
    let numberFormat =
        NumberLiteralOptions.AllowPlusSign
        ||| NumberLiteralOptions.AllowMinusSign
        ||| NumberLiteralOptions.AllowFraction
        
    let parseNumber =
        numberLiteral numberFormat "number"
        |>> fun nl ->
                if nl.IsInteger then Integer (int32 nl.String)
                else Float (float nl.String)
        .>> spaces
        
    let parseString =
        // TODO: Support escape. Ending should be (<> \" && ")
        between (pchar '"') (pchar '"') (manySatisfy ((<>) '"'))
        |>> String
        .>> spaces
        
    let parseAtom =
        choice [
            parseNumber
            parseString
            parseNil
            parseSymbol
        ]
        
    let rec parseExpr (stream : CharStream<_>) : Reply<SExpr> =
        parseAtom stream
        |> fun reply ->
            if reply.Status = ReplyStatus.Ok
            then reply
            else parseList stream
    and parseList (stream : CharStream<_>) : Reply<SExpr> =
       // TODO: Support escaping
       between (pchar '(' .>> spaces) (pchar ')' .>> spaces) (many parseExpr) stream
       |> fun reply ->
           if reply.Status = ReplyStatus.Ok
           then Reply(SExpr.List reply.Result)
           else
               Reply(reply.Status, reply.Error)
       
    let parse (str : string) =
        run parseExpr str

    let parseOrFail (str : string) =
        match parse str with
        | Success (v, _, _) -> v
        | Failure (err, _err, _state) -> failwith err
        
        
module Delispify =
    let rec toFSharp (e : SExpr) : obj =
        match e with
        | SExpr.Nil -> () :> obj
        | SExpr.Character x -> x :> obj
        | SExpr.String x -> x :> obj
        | SExpr.Integer x -> x :> obj
        | SExpr.Float x -> x :> obj
        | SExpr.Symbol x ->
            if x = "t"
            then true :> obj
            else x :> obj
        | SExpr.List xs ->
            // Homogenous list should be ('a list) :> obj rather than (obj list) :> obj
            let isHomogenous =
                xs
                |> Seq.map sexprTag
                |> Seq.distinct
                |> Seq.length
                |> fun l -> l = 1
            if isHomogenous then
                // NOTE: The strange matching and invalidOp is to avoid
                // a warning stating patterns not handled. We already know
                // the list is homogenous, but the compiler doesn't. It's
                // only possible to disable this warning for the entire
                // file or project.
                match xs with
                | (SExpr.String _::_) ->
                    xs |> List.map (function | SExpr.String x -> x | _ -> invalidOp "bug") :> obj
                | (SExpr.Integer _::_) ->
                    xs |> List.map (function | SExpr.Integer x -> x | _ -> invalidOp "bug") :> obj
                | (SExpr.Float _::_) ->
                    xs |> List.map (function | SExpr.Float x -> x | _ -> invalidOp "bug") :> obj
                | (SExpr.Character _::_) ->
                    xs |> List.map (function | SExpr.Character x -> x | _ -> invalidOp "bug") :> obj
                | xs ->
                    xs |> List.map toFSharp :> obj
            else
                xs
                |> List.map toFSharp
                :> obj
                
                
module Lispify =
    type LispifiedMethod = obj -> SExpr list -> SExpr
    let rec fromFSharp (o : obj) : SExpr =
        if isNull o then SExpr.Nil
        // FSharp doesn't allow pattern matching on the Void type,
        // so we have to check it outside the match
        elif o.GetType() = typeof<Void>
        then SExpr.Nil
        else
        match o with
        | :? SExpr as x -> x
        | :? unit -> SExpr.Nil
        | :? bool as x -> if x then SExpr.Symbol "t" else SExpr.Nil
        | :? string as x -> SExpr.String x
        | :? int8 as x -> SExpr.Integer (int x)
        | :? int16 as x -> SExpr.Integer (int x)
        | :? int32 as x -> SExpr.Integer x
        | :? int64 as x -> SExpr.Integer (int x)
        | :? float as x -> SExpr.Float x
        | :? float32 as x -> SExpr.Float (float x)
        | :? decimal as x -> SExpr.Float (float x)
        | :? List<string> as xs -> xs |> List.map SExpr.String |> SExpr.List
        | :? List<int8> as xs -> xs |> List.map (int >> SExpr.Integer) |> SExpr.List
        | :? List<int16> as xs -> xs |> List.map (int >> SExpr.Integer) |> SExpr.List
        | :? List<int32> as xs -> xs |> List.map SExpr.Integer |> SExpr.List
        | :? List<int64> as xs -> xs |> List.map (int >> SExpr.Integer) |> SExpr.List
        | :? List<float> as xs -> xs |> List.map SExpr.Float |> SExpr.List
        | :? List<float32> as xs -> xs |> List.map (float >> SExpr.Float) |> SExpr.List
        | :? List<decimal> as xs -> xs |> List.map (float >> SExpr.Float) |> SExpr.List
        | :? List<obj> as xs -> xs |> List.map fromFSharp |> SExpr.List
        | x -> failwithf $"Unable to convert FSharp value %A{x} to an SExpr"
        
    let lispifyMethod (m : MethodInfo) : LispifiedMethod =
        let mParams = m.GetParameters()
        fun (onObj : obj) (eParams : SExpr list) ->
            // Convert to the exact type. Cannot send int for int8 parameter
            let iParams =
                Seq.zip mParams eParams
                |> Seq.map (fun (p, e) ->
                    if p.ParameterType = typeof<SExpr>
                    then e :> obj
                    else Convert.ChangeType(Delispify.toFSharp e, p.ParameterType)
                )
                |> Array.ofSeq
            let res = m.Invoke(onObj, iParams)
            fromFSharp res
            

[<AutoOpen>]
module Convert =
    let methodToLispifiedMethod (m : MethodInfo) : Lispify.LispifiedMethod =
        Lispify.lispifyMethod m
        
    let stringToSExpr (s : String) : SExpr =
        Parser.parseOrFail s
    
    let sexprToString (e : SExpr) : string =
        Printer.printExpr e
        
    let sexprToFSharp (e : SExpr) : obj =
        Delispify.toFSharp e
        
    let fsharpToSExpr (o : obj) : SExpr =
        Lispify.fromFSharp o


module Env =
    type Env = {
        functions : Map<string, Lispify.LispifiedMethod>
    } with
        static member Empty = {
            functions = Map.empty
        }
    
    let allMethodsFromAssembly (asm : Assembly) =
        seq {
            for t in asm.GetTypes() do
              for m in t.GetMethods() do
                yield m
        }
        
    let justExposed (m : MethodInfo) : MethodInfo option =
        m.CustomAttributes
        |> Seq.tryFind (fun a ->
            // NOTE: We use name rather than checking type+IsAssignableTo
            // in case someone doesn't want to include our library
            // because of licensing issues.
            //    a.AttributeType.IsAssignableTo(typeof<ExposeToEmacs>)
            a.AttributeType.Name.StartsWith("ExposeToEmacs")
            )
        |> Option.map (fun _ -> m)
        
    let justStatic (m : MethodInfo) : MethodInfo option =
        if m.IsStatic then Some m else None
        
    let allFunctionsFromAssembly (asm : Assembly) =
        allMethodsFromAssembly asm
        |> Seq.choose justStatic
        
    let exposedFunctionsFromAssembly (asm : Assembly) : MethodInfo seq =
        allFunctionsFromAssembly asm
        |> Seq.choose justExposed
        
    let toLispified (m : MethodInfo) : (string * Lispify.LispifiedMethod) =
        let fq =
            m.CustomAttributes
            |> Seq.tryFind (fun a ->
                // NOTE: Use string rather than type in case users want
                // to supply their own type to avoid licensing issues
                a.AttributeType.Name = "ExposeToEmacsAsTopLevelKebabCase"
                //a.AttributeType = typeof<ExposeToEmacsAsTopLevelKebabCase>
                )
            |> Option.map (fun _ -> toKebabCase m.Name)
            |> Option.defaultWith (fun () -> $"{m.DeclaringType.FullName}.{m.Name}".Replace('.', '/'))
        (fq, methodToLispifiedMethod m)
        
    let withExposedFunctionsFromAssemblies (asms : Assembly seq) : Env =
        let functions =
            asms
            |> Seq.collect exposedFunctionsFromAssembly
            |> Seq.map toLispified
            |> Map.ofSeq
        { Env.functions = functions }
        
    let withAllFunctionsFromAssemblies (asms : Assembly seq) : Env =
        let functions =
            asms
            |> Seq.collect allFunctionsFromAssembly
            |> Seq.map toLispified
            |> Map.ofSeq
        { Env.functions = functions }
    

module Eval =
    open Env
    
    let unknownFunction (name : string) =
        stringToSExpr $"""(dotnet :error unknown-function :function-name "{name}")"""
    
    let evalExpr (env : Env) (expr : SExpr) : Result<SExpr, string> =
        match expr with
        | FunCall (Symbol f, xs) ->
            Map.tryFind f env.functions
            |> Option.map (fun fn -> Result.Ok (fn null xs))
            |> Option.defaultWith (fun () -> Result.Error $"function %s{f} not in environment")
        | SExpr.List [] ->
            Result.Ok SExpr.Nil
        | _ ->
            Result.Ok expr
            
    let rec eval (env : Env) (strExpr : string) : Result<SExpr list, string> =
        match Parser.parse strExpr with
        | ParserResult.Success (parsed, state, pos) ->
            parsed
            |> evalExpr env
            |> Result.bind (fun ok ->
                if int pos.Index < strExpr.Length
                then
                    let rest = eval env strExpr.[int pos.Index..]
                    rest
                    |> Result.bind (fun oks -> Result.Ok (ok :: oks))
                else Result.Ok [ok]
            )
        | ParserResult.Failure (msg, err, state) ->
            Result.Error msg
    
    let evalOrFail (env : Env) (strExpr : string) : SExpr list =
        match eval env strExpr with
        | Result.Ok exprs -> exprs
        | Result.Error err -> failwithf $"Eval failed: %s{err}"
