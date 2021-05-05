// https://pastebin.com/W7PUQF82
module Simendsjo.EmacsDotNet.SocketServer

open System
open System.Net
open System.Net.Sockets
open System.Threading
 
let listen (address : IPAddress) (port : int) (accepted : (Socket -> unit)) : IDisposable =
  let sockets = ResizeArray()
  let endpoint = IPEndPoint(address, port)
  let cts = new CancellationTokenSource()
  let listener = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
  listener.Bind(endpoint)
  listener.Listen(int SocketOptionName.MaxConnections)
  let rec loop() = async {
    let! socket = Async.FromBeginEnd(listener.BeginAccept, listener.EndAccept)
    sockets.Add(socket)
    accepted socket
    return! loop()
  }
  Async.Start(loop(), cancellationToken = cts.Token)
  { new IDisposable with
      member x.Dispose() =
        cts.Cancel()
        for socket in sockets do
          socket.Shutdown(SocketShutdown.Both)
          socket.Close()
        listener.Shutdown(SocketShutdown.Both)
        listener.Close()
  }