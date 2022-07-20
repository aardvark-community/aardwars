namespace Elm

open System.Threading
open System.Collections.Concurrent
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.Base
open Adaptify
open Aardvark.Rendering
open Aardvark.Application
open System

type Environment<'msg> =
    abstract Emit : #seq<'msg> -> unit
    abstract Window : IRenderWindow
    abstract Runtime : IRuntime
    abstract Shutdown : unit -> unit
    abstract StartTimer : interval : int * action : (unit -> unit) -> IDisposable

type App<'model, 'mmodel, 'message> =
    {
        initial : Environment<'message> -> 'model
        update : Environment<'message> -> 'model -> 'message -> 'model
        view : Environment<'message> -> 'mmodel -> ISg
        unpersist : Unpersist<'model, 'mmodel>
    }

module App =
    
    let inline create (initial : Environment<'message> -> 'model) (update : Environment<'message> -> 'model -> 'message -> 'model) (view : Environment<'message> -> 'mmodel -> ISg) =
        {
            initial = initial
            update = update
            view = view
            unpersist = Unpersist.instance
        }


    let run (window : IRenderWindow) (app : App<'model, 'mmodel, 'message>) =
        let messageQueue = new BlockingCollection<seq<'message>>()
        let mutable updateThread : Thread = null
        let threads = System.Collections.Concurrent.ConcurrentHashSet<IDisposable>()
        let env =
            { new Environment<'message> with
                member x.Emit(msgs : #seq<'message>) =
                    if not messageQueue.IsAddingCompleted then
                        messageQueue.Add (msgs :> seq<_>)
                member x.Window =
                    window
                member x.Runtime =
                    window.Runtime
                member x.Shutdown() =
                    messageQueue.CompleteAdding()
                    if not (isNull updateThread) then
                        updateThread.Join()
                    threads |> Seq.iter (fun d -> d.Dispose())

                member x.StartTimer(interval : int, action : unit -> unit) =
                    let timer = new MultimediaTimer.Trigger(interval)
                    let mutable running = true
                    let thread =
                        startThread <| fun () ->
                            while running do
                                timer.Wait()
                                action()
                    
                    let bla = 
                         { new IDisposable with
                            member x.Dispose() =    
                                running <- false
                                thread.Join()
                                timer.Dispose()
                        }   
                    threads.Add bla |> ignore
                    { new IDisposable with
                        member x.Dispose() =
                            threads.Remove bla |> ignore
                            bla.Dispose()
                    }
            }
        
        let mutable model = app.initial env
        let mmodel = app.unpersist.init model

        updateThread <-
            startThread <| fun () ->
                for msgs in messageQueue.GetConsumingEnumerable() do
                    transact (fun () ->
                        for msg in msgs do
                            model <- app.update env model msg

                        app.unpersist.update mmodel model
                    )


        let scene = app.view env mmodel
        let task = Sg.compile' window.Runtime window.FramebufferSignature false scene
        window.RenderTask <- task
        window.Run()
        env.Shutdown()

module Passes =
    let pass0 = (RenderPass.after "main" RenderPassOrder.Arbitrary RenderPass.main)
    let pass1 = (RenderPass.after "shots" RenderPassOrder.Arbitrary pass0)
    let pass2 = (RenderPass.after "weapon" RenderPassOrder.Arbitrary pass1)
    let pass3 = (RenderPass.after "text" RenderPassOrder.Arbitrary pass2)
                

open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
type NetworkCommand =
    | Connect of name : string
    | UpdatePosition of V3d
    | Hit of playerName : string * damage : float
    | Died of byPlayer : string
    | Stats
    | SpawnShotTrails of list<Line3d * float * float>


[<RequireQualifiedAccess>]
type NetworkMessage =
    | Stats of Map<string, int>
    | UpdatePosition of playerName : string * pos : V3d
    | SpawnShotTrails of list<Line3d * float * float>
    | Connected of playerName : string
    | Disconnected of playerName : string
    | Died of playerName : string
    | Hit of byPlayer : string * damage : float

module NetworkMessage =

    let rx = Regex @"^#([a-zA-Z_]+)[ \t]+(.*)$"


    let pickle (msg : NetworkMessage) =
        match msg with
        | NetworkMessage.Stats s ->
            s |> Seq.map (fun (KeyValue(n, cnt)) -> sprintf "%s:%d" n cnt) |> String.concat "," |> sprintf "#stats %s"
        | NetworkMessage.SpawnShotTrails trails -> 
            sprintf 
                "#spawntrails %s" 
                    (trails |> List.map (fun (l,s,d) -> 
                        sprintf "%f:%f:%f:%f:%f:%f:%f:%f" l.P0.X l.P0.Y l.P0.Z l.P1.X l.P1.Y l.P1.Z s d
                    ) |> String.concat ",") 
        | NetworkMessage.UpdatePosition(n, p) ->
            sprintf "#update %s,%f,%f,%f" n p.X p.Y p.Z
        | NetworkMessage.Connected(n) ->
            sprintf "#connected %s" n
        | NetworkMessage.Disconnected(n) ->
            sprintf "#disconnected %s" n
        | NetworkMessage.Died(n) ->
            sprintf "#died %s" n
        | NetworkMessage.Hit(p,d) ->
            sprintf "#hit %s,%f" p d

    let unpickle (str : string) =
        try
            let m = rx.Match str
            if m.Success then
                let cmd = m.Groups.[1].Value
                let data = m.Groups.[2].Value.Split(',',StringSplitOptions.RemoveEmptyEntries)
                match cmd with
                | "update" ->
                    NetworkMessage.UpdatePosition(data.[0], V3d(float data.[1], float data.[2], float data.[3])) |> Some
                | "connected" ->
                    NetworkMessage.Connected data.[0] |> Some
                | "disconnected" ->
                    NetworkMessage.Disconnected data.[0] |> Some
                | "died" ->
                    NetworkMessage.Died data.[0] |> Some
                | "hit" ->
                    NetworkMessage.Hit (data.[0], float data.[1]) |> Some
                | "stats" ->
                    let stats = 
                        data |> Array.map (fun s -> 
                            let arr = s.Split(':')
                            (arr.[0], int arr.[1])
                        ) |> Map.ofArray
                    NetworkMessage.Stats stats |> Some
                | "spawntrails" -> 
                    let trails = 
                        data |> Array.map (fun d -> 
                            let fs = d.Split(':') |> Array.map float
                            let l = Line3d(V3d(fs.[0],fs.[1],fs.[2]),V3d(fs.[3],fs.[4],fs.[5]))
                            let s = fs.[6]
                            let d = fs.[7]
                            l,s,d
                        ) |> Array.toList
                    NetworkMessage.SpawnShotTrails trails |> Some
                | cmd ->
                    printfn "BAD MSG: %A" cmd
                    None
            else
                printfn "BAD MSG: %A" str
                None
        with e ->
            printfn "BAD MSG: %A (%A)" str e
            None

module NetworkCommand =
    let rx = Regex @"^#([a-zA-Z_]+)[ \t]+(.*)$"
    let pickle (cmd : NetworkCommand) =
        match cmd with
        | NetworkCommand.Connect n -> sprintf "#connect %s" n
        | NetworkCommand.Stats -> "#stats"
        | NetworkCommand.Died cause -> sprintf "#died %s" cause
        | NetworkCommand.UpdatePosition p -> sprintf "#update %f,%f,%f" p.X p.Y p.Z
        | NetworkCommand.Hit(p, d) -> sprintf "#hit %s,%f" p d
        | NetworkCommand.SpawnShotTrails trails -> 
            sprintf 
                "#spawntrails %s" 
                    (trails |> List.map (fun (l,s,d) -> 
                        sprintf "%f:%f:%f:%f:%f:%f:%f:%f" l.P0.X l.P0.Y l.P0.Z l.P1.X l.P1.Y l.P1.Z s d
                    ) |> String.concat ",") 
    let unpickle (str : string) =
        try
            let m = rx.Match str
            if m.Success then
                let cmd = m.Groups.[1].Value
                let data = m.Groups.[2].Value.Split(',',StringSplitOptions.RemoveEmptyEntries)
                match cmd with
                | "connect" -> NetworkCommand.Connect data.[0] |> Some
                | "stats" -> NetworkCommand.Stats |> Some
                | "died" -> NetworkCommand.Died data.[0] |> Some
                | "update" -> NetworkCommand.UpdatePosition(V3d(float data.[0], float data.[1], float data.[2]))|> Some
                | "hit" -> NetworkCommand.Hit(data.[0], float data.[1]) |> Some
                | "spawntrails" -> 
                    let trails = 
                        data |> Array.map (fun d -> 
                            let fs = d.Split(':') |> Array.map float
                            let l = Line3d(V3d(fs.[0],fs.[1],fs.[2]),V3d(fs.[3],fs.[4],fs.[5]))
                            let s = fs.[6]
                            let d = fs.[7]
                            l,s,d
                        ) |> Array.toList
                    Some (NetworkCommand.SpawnShotTrails trails)
                | _ -> 
                    printfn "BAD CMD: %A" cmd
                    None
                
            else
                printfn "BAD CMD: %A" str
                None
        with e ->
            printfn "BAD CMD: %A (%A)" str e
            None
     
type NetworkClient =
    {
        receive : IObservable<NetworkMessage>
        send : NetworkCommand -> unit
    }

module NetworkGroup =
    open System.IO
    open System.Net
    open System.Net.Sockets
    open System.Collections.Concurrent

    let server (port : int) =
        let listener = new TcpListener(IPAddress.Any, port)
        let clients = ConcurrentDictionary<string, TextWriter>()
        let frags = ConcurrentDictionary<string, int>()

        let run =   
            async {
                do! Async.SwitchToThreadPool()
                let pingTask = 
                    async {
                        while true do 
                            do! Async.Sleep 100
                            for KeyValue(name, c) in clients do
                                let s = frags |> Seq.map (fun (KeyValue(name, cnt)) -> name,cnt) |> Map.ofSeq
                                lock c (fun _ -> 
                                    try 
                                        c.WriteLine (NetworkMessage.pickle (NetworkMessage.Stats s))
                                    with e -> 
                                        ()
                                )

                    } |> Async.StartAsTask
                while true do
                    let! c = listener.AcceptTcpClientAsync() |> Async.AwaitTask
                    //c.NoDelay <- true
                    let clientTask = 
                        Async.StartAsTask <|
                            async {
                                use s = c.GetStream()
                                use r = new StreamReader(s)
                                let w = new StreamWriter(s)

                                let inline send (dst : TextWriter) (msg : NetworkMessage) =
                                    lock dst (fun () ->
                                        try dst.WriteLine (NetworkMessage.pickle msg)
                                        with _ -> ()
                                    )
                                    
                                    


                                w.AutoFlush <- true
                                let! msg = r.ReadLineAsync() |> Async.AwaitTask
                                match NetworkCommand.unpickle msg with
                                | Some (NetworkCommand.Connect clientId) ->
                                
                                    for KeyValue(name, c) in clients do
                                        send c (NetworkMessage.Connected clientId)

                                    clients.TryAdd(clientId, w) |> ignore

                                    let inline broadcast msg =
                                        for KeyValue(name, c) in clients do 
                                            if name <> clientId then
                                                send c msg
                                        

                                    try
                                        while true do
                                            let msg = r.ReadLine()
                                            match NetworkCommand.unpickle msg with
                                            | Some cmd ->
                                                match cmd with
                                                | NetworkCommand.UpdatePosition p ->
                                                    broadcast (NetworkMessage.UpdatePosition(clientId, p))
                                                | NetworkCommand.Died cause ->
                                                    frags.AddOrUpdate(cause, (fun _ -> 1), (fun _ o -> o + 1)) |> ignore
                                                    broadcast (NetworkMessage.Died(clientId))
                                                | NetworkCommand.Connect _ ->
                                                    broadcast (NetworkMessage.Connected(clientId))
                                                | NetworkCommand.Hit(other, dmg) ->
                                                    match clients.TryGetValue other with
                                                    | (true, o) ->
                                                        send o (NetworkMessage.Hit(clientId, dmg))
                                                    | _ ->
                                                        ()
                                                | NetworkCommand.Stats ->
                                                    let s = frags |> Seq.map (fun (KeyValue(name, cnt)) -> name,cnt) |> Map.ofSeq
                                                    send w (NetworkMessage.Stats s)
                                                | NetworkCommand.SpawnShotTrails trails ->
                                                    broadcast (NetworkMessage.SpawnShotTrails trails)

                                            | None ->
                                                ()

                                    with _ ->
                                    
                                        clients.TryRemove clientId |> ignore
                                        for KeyValue(name, c) in clients do
                                            try c.WriteLine(sprintf "%s disconnected" clientId); c.Flush()
                                            with _ -> ()
                                | _ ->
                                    ()
                            }
                    ()
            }


        Async.Start run
        listener.Start()

    let client (server : string) (port : int) =
        let id = System.Environment.MachineName
        let received = EventSource<NetworkMessage>()
        let c = new TcpClient()
        //c.NoDelay <- true
        do c.Connect(server, port)
        let s = c.GetStream()
        let w = new StreamWriter(s)
        w.AutoFlush <- true

        let send cmd =
            lock w (fun () ->
                let str = NetworkCommand.pickle cmd
                w.WriteLine str
            )

        send (NetworkCommand.Connect id)

        let receiver =
            Async.StartAsTask <| 
                async {
                    use r = new StreamReader(s)
                    while true do
                        let! msg = r.ReadLineAsync() |> Async.AwaitTask
                        match NetworkMessage.unpickle msg with
                        | Some msg -> received.Emit msg
                        | None -> ()
                }

        { receive = received.Values; send = send }
    


