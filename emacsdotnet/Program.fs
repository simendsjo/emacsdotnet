module emacsdotnet.Program

open System
open System.Net
open System.Reflection

open Simendsjo.EmacsDotNet.Lisp
open Simendsjo.EmacsDotNet.Repl

open Argu
    
[<ExposeToEmacsAsTopLevelKebabCase>]
let echoSexpr (expr : SExpr) = expr
    
let defaultHost = IPAddress.Any
let defaultPort = 5000

type CLIArguments =
    | Host of host:string
    | Port of port:int
    | Expose_Assembly of fullpath:string * justExposed:bool
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Host _ -> $"Listen on host. Defaults to %A{defaultHost}"
            | Port _ -> $"Listen on port. Defaults to %A{defaultPort}"
            | Expose_Assembly _ -> "Load functions from assembly. Full path to the dll, and true/false indicated if only functions tagged ExposeToEmacs should be visible"
    
[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CLIArguments>(programName = "emacsdotnet.exe")
    try
        let results = parser.Parse(argv, raiseOnUsage=true)
        
        let env =
            results.GetResults Expose_Assembly
            |> Seq.collect (fun (path, justExposed) ->
                Env.allFunctionsFromAssembly (Assembly.LoadFrom(path))
                |> Seq.choose (if justExposed then Env.justExposed else Some)
            )
            |> Seq.append (Env.exposedFunctionsFromAssembly (Assembly.GetExecutingAssembly()))
            |> Seq.map Env.toLispified
            |> Map.ofSeq
            |> fun fns -> { Env.Env.Empty with functions = fns }
            
        use server =
            let host = defaultArg (results.TryGetResult Host |> Option.map IPAddress.Parse) defaultHost
            let port = defaultArg (results.TryGetResult Port) defaultPort
            startReplServer env host port
       
        printfn "Press C-c to abort"
        while true do Console.ReadLine() |> ignore
    with e -> printfn $"%s{e.Message}"
    0
