module Simendsjo.EmacsDotNet.Repl

open System
open System.Net
open System.Net.Sockets
open System.Text

open Simendsjo.EmacsDotNet.Lisp

let private startAsyncRepl (env : Env.Env) (socket : Socket) =
    let buf = Array.zeroCreate<byte> 4096
    // TODO: Handle clients who closed the connection
    // TODO: Dispose when done
    let stream = new NetworkStream(socket, false)
    let rec loop() = async {
        let! n = stream.AsyncRead(buf)
        // TODO: What if the buffer wasn't large enough?
        let data = buf.[0..n-1]
        let str = Encoding.UTF8.GetString(data)
        try
            match Eval.eval env str with
            | Result.Ok es -> 
                es
                |> Seq.iter (fun e ->
                    e
                    |> sexprToString
                    |> Encoding.UTF8.GetBytes
                    |> stream.AsyncWrite
                    |> Async.RunSynchronously
                )
            | Result.Error err ->
                // TODO: error message for emacs
                let err = Encoding.UTF8.GetBytes("\"" + err + "\"")
                stream.AsyncWrite(err) |> Async.RunSynchronously
        with ex ->
            // TODO: error message for emacs
            let err = Encoding.UTF8.GetBytes("\"" + ex.ToString() + "\"")
            stream.AsyncWrite(err) |> Async.RunSynchronously
        return! loop()
    }
    Async.Start(loop())
    
let startReplServer (env : Env.Env) (adr : IPAddress) (port : int) =
    printfn $"Starting repl server at %A{adr}:%d{port} with %d{env.functions.Count} exposed functions"
    SocketServer.listen adr port (startAsyncRepl env)
