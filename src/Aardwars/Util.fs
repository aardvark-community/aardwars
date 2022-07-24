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
open Screenshot
open System.IO

#nowarn "3391" //implicit conversion DateTime
    
module GrabNextFrame =
    open OpenTK.Graphics.OpenGL4

    let task (grab : ref<int>) (callback : PixImage<byte> -> unit)= 
        RenderTask.custom (fun (_,_,o) ->
            let take = 
                lock grab (fun () ->
                    let c = grab.Value
                    if c > 0 then
                        grab.Value <- c - 1
                        true
                    else    
                        false
                )
            if take then
                let s = o.viewport.Size
                let arr = Array.zeroCreate<byte> (s.X * s.Y * 4)
                GL.ReadPixels(0,0,s.X, s.Y, PixelFormat.Rgba, PixelType.UnsignedByte, arr)

                let img = PixImage<byte>(Col.Format.RGBA, Volume<byte>(arr, VolumeInfo(0L, V3l(s.X, s.Y, 4), V3l(4, 4*s.X, 1))))
                callback (img.Transformed ImageTrafo.MirrorY :?> PixImage<byte>)
        )



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
    let rref = DateTime(2022,07,24,0,0,0,DateTimeKind.Utc)
    let absTimeNow() =
        let t = DateTime.UtcNow - rref
        t.TotalSeconds

    open System.Net
    open System.Net.Sockets
    let myIps = 
        let n = 
            Dns.GetHostEntry(Dns.GetHostName()).AddressList
            |> Array.choose (fun ip -> 
                if ip.AddressFamily=AddressFamily.InterNetwork then
                    Some (ip.ToString())
                else 
                    None
            )
        printfn "my IPs: %A" n
        n
    let port = 7331
    let inline create (initial : Environment<'message> -> 'model) (update : Environment<'message> -> 'model -> 'message -> 'model) (view : Environment<'message> -> 'mmodel -> ISg) =
        {
            initial = initial
            update = update
            view = view
            unpersist = Unpersist.instance
        }
    let colors = [|"yellow"; "pink"; "red"; "purple";  "orange"; "green"; "blue"; "white"; "black"|]

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




        let screenshot = ref 0
        let coll = new System.Collections.Concurrent.BlockingCollection<PixImage<_>>()
        let callback img =  
            coll.Add img

        let takeScreenshot() =
            async {
                do! Async.SwitchToThreadPool()
                lock screenshot (fun () ->
                    screenshot.Value <- screenshot.Value + 1
                )
                return coll.Take()
            }


        let credentials = Screenshot.Credentials.load ()

        window.Keyboard.Down.Values.Add (fun k ->
            match k with
            | Keys.F12 ->
                async {
                    
                    try
                        let! img = takeScreenshot()
                        let buffer = img.ToPngData()
                    
                        let dir = "screenshots"
                        if not (Directory.Exists(dir)) then
                            Directory.CreateDirectory(dir) |> ignore

                        let n = DateTimeOffset.UtcNow
                        let filename = sprintf "%4d%2d%2d%2d%2d%2d%3d.png" n.Year n.Month n.Day n.Hour n.Minute n.Second n.Millisecond
                        File.WriteAllBytes(Path.Combine(dir, filename), buffer)
                   
                        match credentials with
                        | Valid cs -> Screenshot.upload cs [] buffer |> ignore
                        | _ -> ()
                    with e ->
                        Log.error "%A" e
                
                } |> Async.Start
            | _ ->
                ()
        )

        window.RenderTask <- RenderTask.ofList [ task; GrabNextFrame.task screenshot callback ]
        window.Run()
        env.Shutdown()

module Passes =
    let pass0 = (RenderPass.after "main" RenderPassOrder.Arbitrary RenderPass.main)
    let pass1 = (RenderPass.after "shots" RenderPassOrder.Arbitrary pass0)
    let pass2 = (RenderPass.after "weapon" RenderPassOrder.Arbitrary pass1)
    let pass3 = (RenderPass.after "text" RenderPassOrder.Arbitrary pass2)
    let pass4 = (RenderPass.after "overlay" RenderPassOrder.Arbitrary pass3)
    let pass5 = (RenderPass.after "overlayText" RenderPassOrder.Arbitrary pass4)
                

open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
type NetworkCommand =
    | Connect of name : string
    | UpdatePosition of pos:V3d*fw:V3d*w:int*rld:bool
    | Hit of playerName : string * damage : float * sourceDir : V3d * w : int
    | HitWithSlap of playerName : string * damage : float * slap : V3d * sourceDir : V3d * w : int
    | Died of killingPlayer : string * diedPlayer : string * w : int
    | Stats
    | SpawnShotTrails of list<Line3d * float * float * C4b>
    | SpawnProjectiles of list<string * V3d * V3d * float * float * float * float * float>
    | Explode of owner:string*pos:V3d*sr:float*br:float*sd:float*bd:float
    | Restart

[<RequireQualifiedAccess>]
type NetworkMessage =
    | Stats of Map<string, int*int*string>*time:float
    | UpdatePosition of playerName : string * pos:V3d*fw:V3d*w:int*rld:bool
    | SpawnShotTrails of list<Line3d * float * float * C4b>
    | Connected of playerName : string
    | Disconnected of playerName : string
    | Died of killingPlayer : string * diedPlayer : string * w : int
    | Hit of byPlayer : string * damage : float * sourceDir : V3d * w : int
    | HitWithSlap of byPlayer : string * damage : float * slap : V3d * sourceDir : V3d * w : int
    | SpawnProjectiles of list<string * V3d * V3d * float * float * float * float * float>
    | Explode of owner:string*pos:V3d*sr:float*br:float*sd:float*bd:float
    | Restart of newStartTime:float
    | SyncRestartTime of float

module NetworkMessage =

    let rx = Regex @"^#([a-zA-Z_]+)[ \t]+(.*)$"


    let pickle (msg : NetworkMessage) =
        match msg with
        | NetworkMessage.Stats(s,t) ->
            (s |> Seq.map (fun (KeyValue(n, (k,d,c))) -> sprintf "%s:%d:%d:%s" n k d c) |> String.concat "," |> sprintf "#stats %s")+(sprintf ",%f" t)
        | NetworkMessage.SpawnShotTrails trails -> 
            sprintf 
                "#spawntrails %s" 
                    (trails |> List.map (fun (l,s,d,c) -> 
                        sprintf "%f:%f:%f:%f:%f:%f:%f:%f:%i:%i:%i" l.P0.X l.P0.Y l.P0.Z l.P1.X l.P1.Y l.P1.Z s d c.R c.G c.B
                    ) |> String.concat ",") 
        | NetworkMessage.UpdatePosition(n, p, fw, w, rld) ->
            sprintf "#update %s,%f,%f,%f,%f,%f,%f,%d,%b" n p.X p.Y p.Z fw.X fw.Y fw.Z w rld
        | NetworkMessage.Connected(n) ->
            sprintf "#connected %s" n
        | NetworkMessage.SyncRestartTime st ->
            sprintf "#syncrestarttime %f" st
        | NetworkMessage.Restart t -> 
            sprintf "#restart %f" t
        | NetworkMessage.Disconnected(n) ->
            sprintf "#disconnected %s" n
        | NetworkMessage.Died(k,d,w) ->
            sprintf "#died %s,%s,%d" k d w
        | NetworkMessage.Hit(p,d,sd,w) ->
            sprintf "#hit %s,%f,%f,%f,%f,%d" p d sd.X sd.Y sd.Z w
        | NetworkMessage.HitWithSlap(p,d,v,sd,w) ->
            sprintf "#hitwithslap %s,%f,%f,%f,%f,%f,%f,%f,%d" p d v.X v.Y v.Z sd.X sd.Y sd.Z w
        | NetworkMessage.SpawnProjectiles projs -> 
            sprintf
                "#spawnprojectiles %s"
                    (projs |> List.map (fun (n,p,v,d,sr,br,sd,bd) -> 
                        sprintf "%s:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f" n p.X p.Y p.Z v.X v.Y v.Z d sr br sd bd
                    ) |> String.concat ",")
        | NetworkMessage.Explode (n,p,sr,br,sd,bd) -> 
            sprintf "#explode %s,%f,%f,%f,%f,%f,%f,%f" n p.X p.Y p.Z sr br sd bd

    let unpickle (str : string) =
        try
            let m = rx.Match str
            if m.Success then
                let cmd = m.Groups.[1].Value
                let data = m.Groups.[2].Value.Split(',',StringSplitOptions.RemoveEmptyEntries)
                match cmd with
                | "restart" -> 
                    NetworkMessage.Restart(float data.[0]) |> Some
                | "update" ->
                    NetworkMessage.UpdatePosition(data.[0], V3d(float data.[1], float data.[2], float data.[3]), V3d(float data.[4], float data.[5], float data.[6]), int data.[7], System.Boolean.Parse data.[8]) |> Some
                | "connected" ->
                    NetworkMessage.Connected data.[0] |> Some
                | "syncrestarttime" ->
                    NetworkMessage.SyncRestartTime (float data.[0]) |> Some
                | "disconnected" ->
                    NetworkMessage.Disconnected data.[0] |> Some
                | "died" ->
                    NetworkMessage.Died(data.[0],data.[1],int data.[2]) |> Some
                | "hit" ->
                    NetworkMessage.Hit (data.[0], float data.[1], V3d(float data.[2], float data.[3], float data.[4]), int data.[5]) |> Some
                | "hitwithslap" ->
                    NetworkMessage.HitWithSlap (data.[0], float data.[1], V3d(float data.[2], float data.[3], float data.[4]), V3d(float data.[5], float data.[6], float data.[7]), int data.[8]) |> Some
                | "stats" ->
                    let stats = 
                        data |> Array.truncate (data.Length-1) |> Array.map (fun s -> 
                            let arr = s.Split(':')
                            (arr.[0], (int arr.[1], int arr.[2], arr.[3]))
                        ) |> Map.ofArray
                    let t = float data.[data.Length-1]
                    NetworkMessage.Stats (stats,t) |> Some
                | "spawntrails" -> 
                    let trails = 
                        data |> Array.map (fun d -> 
                            let fs = d.Split(':') |> Array.map float
                            let l = Line3d(V3d(fs.[0],fs.[1],fs.[2]),V3d(fs.[3],fs.[4],fs.[5]))
                            let s = fs.[6]
                            let d = fs.[7]
                            let c = C4b(fs.[8], fs.[9], fs.[10])
                            l,s,d,c
                        ) |> Array.toList
                    NetworkMessage.SpawnShotTrails trails |> Some
                | "spawnprojectiles" -> 
                    let projs =
                        data |> Array.map (fun d -> 
                            let fs = d.Split(':')
                            let inline ff i = float fs.[i]
                            let n = fs.[0]
                            let p = V3d(ff 1, ff 2, ff 3)
                            let v = V3d(ff 4, ff 5, ff 6)
                            let d = ff 7
                            let sr = ff 8
                            let br = ff 9
                            let sd = ff 10
                            let bd = ff 11
                            n,p,v,d,sr,br,sd,bd
                        ) |> Array.toList
                    Some (NetworkMessage.SpawnProjectiles projs)
                | "explode" -> 
                    let inline ff i = float data.[i]
                    Some (NetworkMessage.Explode (data.[0],V3d(ff 1, ff 2, ff 3), ff 4, ff 5, ff 6, ff 7))
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
        | NetworkCommand.Stats -> "#stats "
        | NetworkCommand.Restart -> "#restart "
        | NetworkCommand.Died(k,d,w) -> sprintf "#died %s,%s,%d" k d w
        | NetworkCommand.UpdatePosition(p,fw,w,rld) -> sprintf "#update %f,%f,%f,%f,%f,%f,%d,%b" p.X p.Y p.Z fw.X fw.Y fw.Z w rld
        | NetworkCommand.Hit(p, d, sd, w) -> sprintf "#hit %s,%f,%f,%f,%f,%d" p d sd.X sd.Y sd.Z w
        | NetworkCommand.HitWithSlap(p,d,v,sd,w) -> sprintf "#hitwithslap %s,%f,%f,%f,%f,%f,%f,%f,%d" p d v.X v.Y v.Z sd.X sd.Y sd.Z w
        | NetworkCommand.SpawnShotTrails trails -> 
            sprintf 
                "#spawntrails %s" 
                    (trails |> List.map (fun (l,s,d,c) -> 
                        sprintf "%f:%f:%f:%f:%f:%f:%f:%f:%i:%i:%i" l.P0.X l.P0.Y l.P0.Z l.P1.X l.P1.Y l.P1.Z s d c.R c.G c.B
                    ) |> String.concat ",") 
        | NetworkCommand.SpawnProjectiles projs -> 
            sprintf
                "#spawnprojectiles %s"
                    (projs |> List.map (fun (n,p,v,d,sr,br,sd,bd) -> 
                        sprintf "%s:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f:%f" n p.X p.Y p.Z v.X v.Y v.Z d sr br sd bd
                    ) |> String.concat ",")
        | NetworkCommand.Explode (n,p,sr,br,sd,bd) -> 
            sprintf "#explode %s,%f,%f,%f,%f,%f,%f,%f" n p.X p.Y p.Z sr br sd bd

    let unpickle (str : string) =
        try
            let m = rx.Match str
            if m.Success then
                let cmd = m.Groups.[1].Value
                let data = m.Groups.[2].Value.Split(',',StringSplitOptions.RemoveEmptyEntries)
                match cmd with
                | "connect" -> NetworkCommand.Connect data.[0] |> Some
                | "stats" -> NetworkCommand.Stats |> Some
                | "restart" -> NetworkCommand.Restart |> Some
                | "died" -> NetworkCommand.Died(data.[0],data.[1],int data.[2]) |> Some
                | "update" -> NetworkCommand.UpdatePosition(V3d(float data.[0], float data.[1], float data.[2]), V3d(float data.[3], float data.[4], float data.[5]), int data.[6], System.Boolean.Parse data.[7])|> Some
                | "hit" -> NetworkCommand.Hit(data.[0], float data.[1], V3d(float data.[2], float data.[3], float data.[4]), int data.[5]) |> Some
                | "hitwithslap" -> NetworkCommand.HitWithSlap(data.[0], float data.[1], V3d(float data.[2], float data.[3], float data.[4]), V3d(float data.[5], float data.[6], float data.[7]), int data.[8]) |> Some
                | "spawntrails" -> 
                    let trails = 
                        data |> Array.map (fun d -> 
                            let fs = d.Split(':') |> Array.map float
                            let l = Line3d(V3d(fs.[0],fs.[1],fs.[2]),V3d(fs.[3],fs.[4],fs.[5]))
                            let s = fs.[6]
                            let d = fs.[7]
                            let c = C4b(fs.[8], fs.[9], fs.[10])
                            l,s,d,c
                        ) |> Array.toList
                    Some (NetworkCommand.SpawnShotTrails trails)
                | "spawnprojectiles" -> 
                    let projs =
                        data |> Array.map (fun d -> 
                            let fs = d.Split(':')
                            let inline ff i = float fs.[i]
                            let n = fs.[0]
                            let p = V3d(ff 1, ff 2, ff 3)
                            let v = V3d(ff 4, ff 5, ff 6)
                            let d = ff 7
                            let sr = ff 8
                            let br = ff 9
                            let sd = ff 10
                            let bd = ff 11
                            n,p,v,d,sr,br,sd,bd
                        ) |> Array.toList
                    Some (NetworkCommand.SpawnProjectiles projs)
                | "explode" -> 
                    let inline ff i = float data.[i]
                    Some (NetworkCommand.Explode (data.[0],V3d(ff 1, ff 2, ff 3), ff 4, ff 5, ff 6, ff 7))
                | _ -> 
                    printfn "BAD CMD (unknown cmd name): %A" cmd
                    None
                
            else
                printfn "BAD CMD (regex didn't match): %A" str
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

    type ClientInfo =
        {
            c : TextWriter
            i : int
        }
    type DeathInfo =
        {
            killingPlayer : string
            diedPlayer : string
            gun : int
        }
    let server (port : int) =
        let listener = new TcpListener(IPAddress.Any, port)
        let clients = ConcurrentDictionary<string, ClientInfo>()
        let frags = ConcurrentDictionary<string, list<DeathInfo>>()
        let mutable currentRoundStartTime = App.absTimeNow()
        
        let run =   
            async {
                do! Async.SwitchToThreadPool()
                let pingTask = 
                    async {
                        while true do 
                            do! Async.Sleep 100
                            let mycs = (clients |> Seq.map (fun kvp -> kvp.Key,kvp.Value) |> Map.ofSeq)
                            for KeyValue(name, info) in clients do
                                let c = info.c
                                let s = 
                                    clients.Keys |> Seq.map (fun c -> 
                                        let kills = frags.GetValueOrDefault(c,[]) |> List.filter (fun i -> i.diedPlayer <> i.killingPlayer) |> List.length
                                        let deaths = 
                                            frags |> Seq.sumBy (fun kvp -> 
                                                kvp.Value |> List.filter (fun i -> i.diedPlayer=c) |> List.length
                                            )
                                        let color = 
                                            match mycs |> Map.tryFind c with 
                                            | Some i -> i.i
                                            | None -> 0
                                        let col = App.colors.[color%App.colors.Length]
                                        c,(kills,deaths,col)
                                    ) |> Map.ofSeq
                                lock c (fun _ -> 
                                    try 
                                        c.WriteLine (NetworkMessage.pickle (NetworkMessage.Stats(s,currentRoundStartTime)))
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
                                        send c.c (NetworkMessage.SyncRestartTime currentRoundStartTime)
                                        send c.c (NetworkMessage.Connected clientId)
                                    
                                    send w (NetworkMessage.SyncRestartTime currentRoundStartTime)
                                    let i = 
                                        if clients.Count > 0 then 
                                            let kvp = clients |> Seq.maxBy(fun kvp -> kvp.Value.i)
                                            kvp.Value.i+1
                                        else
                                            0
                                    clients.TryAdd(clientId, {c=w;i=i}) |> ignore

                                    let inline broadcast msg =
                                        for KeyValue(name, info) in clients do 
                                            let c = info.c
                                            if name <> clientId then
                                                send c msg
                                        

                                    try
                                        while true do
                                            let msg = r.ReadLine()
                                            match NetworkCommand.unpickle msg with
                                            | Some cmd ->
                                                match cmd with
                                                | NetworkCommand.Restart -> 
                                                    currentRoundStartTime <- App.absTimeNow()
                                                    let ps = frags.Keys
                                                    frags.Clear()
                                                    ps |> Seq.iter (fun n -> frags.AddOrUpdate(n,[],(fun _ _ -> [])) |> ignore)
                                                    send w (NetworkMessage.SyncRestartTime currentRoundStartTime)
                                                    broadcast (NetworkMessage.Restart currentRoundStartTime)
                                                | NetworkCommand.UpdatePosition(p,fw,w,rld) ->
                                                    broadcast (NetworkMessage.UpdatePosition(clientId,p,fw,w,rld))
                                                | NetworkCommand.Died(cause,died,gun) ->
                                                    let v = {diedPlayer=died;killingPlayer=cause;gun=gun}
                                                    frags.AddOrUpdate(cause, (fun _ -> [v]), (fun _ o -> v::o)) |> ignore
                                                    broadcast (NetworkMessage.Died(cause,died,gun))
                                                | NetworkCommand.Connect _ ->
                                                    send w (NetworkMessage.SyncRestartTime currentRoundStartTime)
                                                    broadcast (NetworkMessage.SyncRestartTime currentRoundStartTime)
                                                    broadcast (NetworkMessage.Connected(clientId))
                                                | NetworkCommand.Hit(other, dmg, sd,w) ->
                                                    match clients.TryGetValue other with
                                                    | (true, o) ->
                                                        send o.c (NetworkMessage.Hit(clientId, dmg, sd,w))
                                                    | _ ->
                                                        ()
                                                | NetworkCommand.HitWithSlap(other, dmg, vel, sd,w) ->
                                                    match clients.TryGetValue other with
                                                    | (true, o) ->
                                                        send o.c (NetworkMessage.HitWithSlap(clientId, dmg, vel, sd,w))
                                                    | _ ->
                                                        ()
                                                | NetworkCommand.Stats ->
                                                    let s =
                                                        clients.Keys |> Seq.map (fun c -> 
                                                            let kills = frags.GetValueOrDefault(c,[]) |> List.filter (fun i -> i.diedPlayer <> i.killingPlayer) |> List.length
                                                            let deaths = 
                                                                frags |> Seq.sumBy (fun kvp -> 
                                                                    kvp.Value |> List.filter (fun i -> i.diedPlayer=c) |> List.length
                                                                )
                                                            let color = 
                                                                match (clients |> Seq.map (fun kvp -> kvp.Key,kvp.Value) |> Map.ofSeq) |> Map.tryFind c with 
                                                                | Some i -> i.i
                                                                | None -> 0
                                                            let col = App.colors.[color%App.colors.Length]
                                                            c,(kills,deaths,col)
                                                        ) |> Map.ofSeq
                                                    send w (NetworkMessage.Stats (s,currentRoundStartTime))
                                                | NetworkCommand.SpawnShotTrails trails ->
                                                    broadcast (NetworkMessage.SpawnShotTrails trails)
                                                | NetworkCommand.SpawnProjectiles projs ->
                                                    broadcast (NetworkMessage.SpawnProjectiles projs)
                                                | NetworkCommand.Explode (a,s,d,f,g,h) ->
                                                    broadcast (NetworkMessage.Explode (a,s,d,f,g,h))

                                            | None ->
                                                ()

                                    with _ ->
                                    
                                        clients.TryRemove clientId |> ignore
                                        for KeyValue(name, c) in clients do
                                            let c = c.c
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
    
    
type Marker = Marker
module MapAssets = 
    open System.IO.Compression
    open System.IO
    let ass = typeof<Marker>.Assembly
    let getFromEmbeddedResources() =
        let baseDir = Path.combine [Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData; "Aardwars"]
        let inline ensure d = if not (Directory.Exists(d)) then Directory.CreateDirectory(d) |> ignore
        ensure baseDir
        let extract resourceName folderName =
            let path = Path.combine [baseDir; folderName]
            if not (Directory.Exists(path)) then
                Directory.CreateDirectory(path) |> ignore
                use s = ass.GetManifestResourceStream resourceName
                let a = new ZipArchive(s)
                a.ExtractToDirectory(path)
            path
        let mapPath = extract "Aardwars.Jakobs KitPvP.zip" "Jakobs KitPvP"
        let texPath = extract "Aardwars.textures.zip" "textures"
        texPath, mapPath

module Shader =
    open FShade
    [<ReflectedDefinition>]
    let hsv2rgb (h : float) (s : float) (v : float) =
        let h = Fun.Frac(h)
        let chr = v * s
        let x = chr * (1.0 - Fun.Abs(Fun.Frac(h * 3.0) * 2.0 - 1.0))
        let m = v - chr
        let t = (int)(h * 6.0)
        match t with
        | 0 -> V3d(chr + m, x + m, m)
        | 1 -> V3d(x + m, chr + m, m)
        | 2 -> V3d(m, chr + m, x + m)
        | 3 -> V3d(m, x + m, chr + m)
        | 4 -> V3d(x + m, m, chr + m)
        | 5 -> V3d(chr + m, m, x + m)
        | _ -> V3d(chr + m, x + m, m)

    type TrailVertex = 
        {
            [<Color>] c : V4d
            [<Semantic("Alphas")>] t : float32
        }

    let adjustAlpha (v : TrailVertex) =
        vertex {
            return {v with c = V4d(v.c.X, v.c.Y, v.c.Z, float v.t)}
        }
    let sgAlpha (v : TrailVertex) =
        fragment {
            let a : float32 = uniform?Alpha
            return V4d(v.c.X, v.c.Y, v.c.Z, float a)
        }