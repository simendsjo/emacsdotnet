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
    let parseSymbol =
        many1Chars (anyOf "+-*/_~!@$%^&=:<>{}" <|> letter <|> digit)
        |>> fun s -> if s = "nil" then Nil else Symbol s
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
        
    let parseCharacter =
        skipChar '?' >>. anyChar
        |>> Character
        .>> spaces

    let parseString =
        // TODO: Support escape. Ending should be (<> \" && ")
        between (pchar '"') (pchar '"') (manySatisfy ((<>) '"'))
        |>> String
        .>> spaces
        
    let parseAtom =
        choice [
            parseNumber
            parseCharacter
            parseString
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
    let rec toFSharp (e : SExpr) (toType : Type) : obj =
        let cannotConvert () =
            failwithf $"Cannot convert %A{e} to {toType}"
        let convOrFail (fromType : Type) (x : obj) =
            if toType.IsAssignableFrom(fromType)
            then Convert.ChangeType(x, toType)
            else cannotConvert ()
        match e with
        | _ when toType = typeof<SExpr> ->
            e :> obj
        | SExpr.Nil when toType = typeof<unit> ->
            () :> obj
        | SExpr.Nil when toType = typeof<Void> ->
            // TODO: What is the correct value to use here?
            () :> obj
        | SExpr.Nil when toType = typeof<obj> ->
            // TODO: Should we rather return null here?
            () :> obj
        | SExpr.Nil when toType.Name = "FSharpList`1" ->
            // TODO: Support more collections
            // TODO: Avoid hardcoding all possible permutations
            let elemType = toType.GenericTypeArguments.[0]
            if elemType = typeof<bool> then
                List.empty<bool> :> obj
            elif elemType = typeof<int> then
                List.empty<int> :> obj
            elif elemType = typeof<float> then
                List.empty<float> :> obj
            elif elemType = typeof<char> then
                List.empty<char> :> obj
            elif elemType = typeof<string> then
                List.empty<string> :> obj
            elif elemType = typeof<obj> then
                List.empty<obj> :> obj
            else
                cannotConvert ()
        | SExpr.Nil when (not toType.IsValueType) ->
            null :> obj
        | SExpr.Nil when toType = typeof<bool> ->
            false :> obj
        | SExpr.Symbol "t" when toType = typeof<bool> || toType = typeof<obj> ->
            true :> obj
        | _ when toType = typeof<bool> ->
            cannotConvert ()
        | SExpr.Character x when toType = typeof<char> || toType = typeof<obj>->
            x :> obj
        | _ when toType = typeof<char> ->
            cannotConvert ()
        | SExpr.String x when toType = typeof<string>  || toType = typeof<obj> ->
            x :> obj
        | _ when toType = typeof<string> ->
            cannotConvert ()
        | SExpr.Integer x ->
            convOrFail typeof<int> x
        | SExpr.Float x ->
            convOrFail typeof<float> x
        | SExpr.Symbol s when toType = typeof<string> || toType = typeof<obj> ->
            s :> obj
        | SExpr.Symbol _ ->
            cannotConvert ()
        | SExpr.List xs ->
            // FIXME: Support seq and other collections
            if toType.Name = "FSharpList`1" then
                let elemType = toType.GenericTypeArguments.[0]
                xs
                |> Seq.map (fun x -> toFSharp x elemType)
                |> fun s ->
                    // FIXME: Need to be able to cast to the correct type
                    // without hardcoding all permutations
                    if elemType = typeof<bool> then
                        Convert.ChangeType(s |> Seq.cast<bool> |> List.ofSeq, toType)
                    elif elemType = typeof<int> then
                        Convert.ChangeType(s |> Seq.cast<int> |> List.ofSeq, toType)
                    elif elemType = typeof<float> then
                        Convert.ChangeType(s |> Seq.cast<float> |> List.ofSeq, toType)
                    elif elemType = typeof<char> then
                        Convert.ChangeType(s |> Seq.cast<char> |> List.ofSeq, toType)
                    elif elemType = typeof<string> then
                        Convert.ChangeType(s |> Seq.cast<string> |> List.ofSeq, toType)
                    elif elemType = typeof<obj> then
                        Convert.ChangeType(s |> Seq.cast<obj> |> List.ofSeq, toType)
                    else
                        cannotConvert ()
            else
                cannotConvert ()
        | _ ->
            cannotConvert ()



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
        | :? char as x -> SExpr.Character x
        | :? string as x -> SExpr.String x
        | :? int8 as x -> SExpr.Integer (int x)
        | :? int16 as x -> SExpr.Integer (int x)
        | :? int32 as x -> SExpr.Integer (int x)
        | :? int64 as x -> SExpr.Integer (int x)
        | :? uint8 as x -> SExpr.Integer (int x)
        | :? uint16 as x -> SExpr.Integer (int x)
        | :? uint32 as x -> SExpr.Integer (int x)
        | :? uint64 as x -> SExpr.Integer (int x)
        | :? float as x -> SExpr.Float (float x)
        | :? float32 as x -> SExpr.Float (float x)
        | :? decimal as x -> SExpr.Float (float x)
        | :? seq<char> as xs -> xs |> Seq.map SExpr.Character |> List.ofSeq |> SExpr.List
        | :? seq<bool> as xs -> xs |> Seq.map (fun x -> if x then SExpr.Symbol "t" else SExpr.Nil) |> List.ofSeq |> SExpr.List
        | :? seq<string> as xs -> xs |> Seq.map SExpr.String |> List.ofSeq |> SExpr.List
        | :? seq<int8> as xs -> xs |> Seq.map (int >> SExpr.Integer) |> List.ofSeq |> SExpr.List
        | :? seq<int16> as xs -> xs |> Seq.map (int >> SExpr.Integer) |> List.ofSeq |> SExpr.List
        | :? seq<int32> as xs -> xs |> Seq.map (int >> SExpr.Integer) |> List.ofSeq |> SExpr.List
        | :? seq<int64> as xs -> xs |> Seq.map (int >> SExpr.Integer) |> List.ofSeq |> SExpr.List
        | :? seq<uint8> as xs -> xs |> Seq.map (int >> SExpr.Integer) |> List.ofSeq |> SExpr.List
        | :? seq<uint16> as xs -> xs |> Seq.map (int >> SExpr.Integer) |> List.ofSeq |> SExpr.List
        | :? seq<uint32> as xs -> xs |> Seq.map (int >> SExpr.Integer) |> List.ofSeq |> SExpr.List
        | :? seq<uint64> as xs -> xs |> Seq.map (int >> SExpr.Integer) |> List.ofSeq |> SExpr.List
        | :? seq<float> as xs -> xs |> Seq.map (float >> SExpr.Float) |> List.ofSeq |> SExpr.List
        | :? seq<float32> as xs -> xs |> Seq.map (float >> SExpr.Float) |> List.ofSeq |> SExpr.List
        | :? seq<decimal> as xs -> xs |> Seq.map (float >> SExpr.Float) |> List.ofSeq |> SExpr.List
        | :? seq<obj> as xs -> xs |> Seq.map fromFSharp |> List.ofSeq |> SExpr.List
        | x -> failwithf $"Unable to convert FSharp value %A{x} to an SExpr"

    let sexprToFSharp (sexpr : SExpr) (dest : Type) =
        Delispify.toFSharp sexpr dest

    let sexprToTypedFSharp<'dest> (sexpr : SExpr) : 'dest =
        sexprToFSharp sexpr (typeof<'dest>) :?> 'dest

    let makeSExprToDotnetParamsConverter (mParams : Type seq) : (SExpr seq -> obj array) =
        fun (eParams : SExpr seq) ->
            let iParams =
                Seq.zip mParams eParams
                |> Seq.map (fun (p, e) -> sexprToFSharp e p)
                |> Array.ofSeq
            iParams

    let lispifyMethod (m : MethodInfo) : LispifiedMethod =
        let paramConv =
            m.GetParameters()
            |> Seq.map (fun p -> p.ParameterType)
            |> makeSExprToDotnetParamsConverter
        fun (onObj : obj) (eParams : SExpr list) ->
            let iParams = paramConv eParams
            let res = m.Invoke(onObj, iParams)
            fromFSharp res
            
    let lispifyFSharpFunction (fn : obj) : LispifiedMethod=
        let invoker =
            fn.GetType().GetRuntimeMethods()
            |> Seq.find (fun m -> m.Name = "Invoke")
        let fParams = invoker.GetParameters() |> Seq.map (fun p -> p.ParameterType)
        fun (_ : obj) (e : SExpr list) ->
            let iParams = makeSExprToDotnetParamsConverter fParams e
            let res = invoker.Invoke(fn, iParams)
            fromFSharp res

    let extractFSharpFunctionParamsAndReturn fn =
        let rec go state (p, next) =
            if FSharpType.IsFunction next
            then go (p :: state) (FSharpType.GetFunctionElements next)
            else (List.rev (p :: state), next)
        go [] (FSharpType.GetFunctionElements (fn.GetType()))

    let apply1 (fn : 'a -> 'b) (e : SExpr) : 'b =
        let v = sexprToTypedFSharp<'a> e
        fn v


[<AutoOpen>]
module Convert =
    let methodToLispifiedMethod (m : MethodInfo) : Lispify.LispifiedMethod =
        Lispify.lispifyMethod m
        
    let stringToSExpr (s : String) : SExpr =
        Parser.parseOrFail s
    
    let sexprToString (e : SExpr) : string =
        Printer.printExpr e
        
    let sexprToFSharp (e : SExpr) (toType : Type) : obj =
        Delispify.toFSharp e toType
        
    let fsharpToSExpr (o : obj) : SExpr =
        Lispify.fromFSharp o


module Env =
    open Microsoft.FSharp.Quotations

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
            a.AttributeType.Name.StartsWith(nameof(ExposeToEmacs))
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
                a.AttributeType.Name = nameof(ExposeToEmacsAsTopLevelKebabCase)
                //a.AttributeType = typeof<ExposeToEmacsAsTopLevelKebabCase>
                )
            |> Option.map (fun _ -> toKebabCase m.Name)
            |> Option.defaultWith (fun () -> $"{m.DeclaringType.FullName}.{m.Name}".Replace('.', '/'))
        (fq, methodToLispifiedMethod m)
        
    let rec toLispifiedFromQuotation (q : Expr) : (string * Lispify.LispifiedMethod) =
        match q with
        | Patterns.Call(None, expr, _) ->
          toLispified expr
        | Patterns.Lambda(_, expr) ->
          toLispifiedFromQuotation expr
        | x ->
          failwithf $"Not a function call: %A{x}"

    let withFunction (name : string) (lispified : Lispify.LispifiedMethod) (env : Env) : Env =
      { env with functions = Map.add name lispified env.functions }

    let withFunctionFromQuoted (q : Expr) (env : Env) : Env =
      toLispifiedFromQuotation q
      |> fun (n, l) -> withFunction n l env

    let withFunctionFromMethodInfo (m : MethodInfo) (env : Env) : Env =
      toLispified m
      |> fun (n, l) -> withFunction n l env

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
