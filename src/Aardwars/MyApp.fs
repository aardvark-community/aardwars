﻿namespace Elm

open Adaptify
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open FSharp.Data.Adaptive
open Aardvark.Application
open Elm
open System.Reflection
open Aardwars
open Aardvark.Rendering.Text


type CameraMessage =
    | Look of delta : V2d
    | StartMove of speed : V3d
    | StopMove of speed : V3d
    | UpdateTime of seconds : float * delta : float

module CameraController =
    let initial = 
        {
            move = V3d.Zero
            velocity = V3d.Zero
            look = false
            camera = CameraView.lookAt (V3d(3,4,5)) V3d.Zero V3d.OOI
        }
    
    let update (cam : CameraModel) (msg : CameraMessage) =
        match msg with
        | CameraMessage.Look delta ->
            let o = cam.camera
            let fw = o.Forward
            let r = o.Right
                    
            let dy = 
                if Vec.dot o.Sky fw > 0.991 then max delta.Y 0.0
                elif Vec.dot o.Sky fw < -0.991 then min delta.Y 0.0
                else delta.Y
            let trafo =
                M44d.Rotation(r, float dy * -0.005 ) *
                M44d.Rotation(V3d.OOI, float delta.X * -0.005   )
            let newForward = trafo.TransformDir fw |> Vec.normalize
                    
            let p = o.WithForward newForward
            { cam with camera = p }
        | CameraMessage.StartMove speed ->
            { cam with move = cam.move + speed }
        | CameraMessage.StopMove speed ->
            { cam with move = cam.move - speed }
        | CameraMessage.UpdateTime(_, dt) ->
            //let vNew = Fun.Lerp(0.5 ** dt, cam.velocity, cam.move)
            //let v = V3d(vNew.X, vNew.Y, cam.velocity.Z)

            let v = cam.velocity
            if v.AllEqual 0.0 then
                cam
            else
                let o = cam.camera
                let sky = o.Sky |> Vec.normalize
                let f = (o.Forward - sky * Vec.dot o.Forward sky) |> Vec.normalize
                let r = (o.Right - sky * Vec.dot o.Right sky) |> Vec.normalize

                { cam with 
                    camera = 
                        o.WithLocation(
                            o.Location + 
                            f * v.Y * dt +
                            r * v.X * dt +
                            sky * v.Z * dt
                        ) 
                }
     

type Message =
    | MouseMove of delta : V2d
    | MouseDown of button : MouseButtons
    | MouseUp of button : MouseButtons
    | KeyDown of key : Keys
    | KeyUp of key : Keys
    | Resize of newSize : V2i
    | UpdateTime of seconds : float * delta : float

module Game =

    let events (env : Environment<Message>) =
        let win = env.Window
        let glfw = win.GetType().GetField("glfw", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> Silk.NET.GLFW.Glfw
        let hwin = win.GetType().GetField("win", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> nativeptr<Silk.NET.GLFW.WindowHandle>
        let mutable mouseDelta = V2d.Zero
        win.Cursor <- Cursor.None
    
        win.Mouse.Move.Values.Add(fun (o,n) ->
            let c = V2d (AVal.force win.Sizes) / 2.0
            let mutable px = 0.0
            let mutable py = 0.0
            glfw.GetCursorPos(hwin, &px, &py)
            glfw.SetCursorPos(hwin, c.X, c.Y)
            env.Emit [ MouseMove (V2d(px, py) - c) ]
        )

        win.Mouse.Down.Values.Add(fun b -> 
            env.Emit [ MouseDown b ]
        )
        
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let mutable last = sw.Elapsed.TotalSeconds
        let timer = 
            env.StartTimer(4, fun () ->
                let now = sw.Elapsed.TotalSeconds
                env.Emit [UpdateTime(now, now - last)]
                last <- now
            )

        env.Window.Keyboard.Down.Values.Add(fun k ->
            env.Emit [KeyDown k]
        )
        env.Window.Keyboard.Up.Values.Add(fun k ->
            env.Emit [KeyUp k]
        )

        env.Window.Sizes.AddCallback (fun s ->
            env.Emit [Resize s]
        ) |> ignore


   //let randomTargets = 
   //    type Target =
   //{
   //    currentHp : int
   //    maxHp : int
   //    pos : V3d
   //    radius : float
   //}
       
        
             
             
             
        


    let intitial (env : Environment<Message>) = 
    
        let blockTable = Block.loadMapping @"C:\Users\Schorsch\Desktop\mc"

        let sections =
            //@"T:\Dropbox\Data\minecraft\Notre_Dame_and_Medieval_City\Notre Dame and Medieval City"
            @"C:\Users\Schorsch\Downloads\Notre Dame and Medieval City"
            |> Minecraft.getRegions
            //|> Seq.take 1
            |> Seq.collect Minecraft.enumerateRawNbtBuffers
            |> Seq.map Minecraft.parseRawNbtBuffer
            |> Seq.collect Minecraft.extractSectionsFromChunk
            |> Seq.toArray

        let mutable tree = Octree.empty 1024 (fun (bi : BoxInfo) -> bi.BoundingBox)

        let states = System.Collections.Generic.HashSet()
        let mutable iter = 0

        let textureList = System.Collections.Generic.List()
        let textureDict = Dict<string, int>()

        let getTextureId(name : string) =
            textureDict.GetOrCreate(name, fun name ->
                let id = textureList.Count
                textureList.Add name
                id
            )

        sections 
        |> Array.iter (fun s ->
            let off = V3s(s.XPos, s.ZPos, s.Y) * 16s
                
            let exists x y z =
                if x < 0 || y < 0 || z < 0 || x > 15 || y > 15 || z > 15 then 
                    false
                else
                    let idx = y*256 + z*16 + x
                    not (s.BlockStates.[idx].Contains "air")
                    //s.BlockStates.[idx] <> "minecraft:air"

            let chunk =
                [|
                    let mutable i = 0
                    for y in 0 .. 15 do
                        for z in 0 .. 15 do
                            for x in 0 .. 15 do
                                states.Add s.BlockStates.[i] |> ignore
                                if not (s.BlockStates.[i].Contains "air") then
                                    let occluded =
                                        exists (x - 1) y z &&
                                        exists (x + 1) y z &&
                                        exists x (y + 1) z &&
                                        exists x (y - 1) z &&
                                        exists x y (z + 1) &&
                                        exists x y (z - 1) &&
                                        
                                        exists (x - 1) (y + 1) z &&
                                        exists (x + 1) (y + 1) z &&
                                        exists (x - 1) (y - 1) z &&
                                        exists (x + 1) (y - 1) z &&
                                        
                                        exists (x - 1) y (z + 1) &&
                                        exists (x + 1) y (z + 1) &&
                                        exists (x - 1) y (z - 1) &&
                                        exists (x + 1) y (z - 1) &&
                                        
                                        exists x (y - 1) (z + 1) &&
                                        exists x (y + 1) (z + 1) &&
                                        exists x (y - 1) (z - 1) &&
                                        exists x (y + 1) (z - 1)

                                    if not occluded then
                                        let mat = s.BlockStates.[i]
                                        let info = 
                                            match blockTable.TryGetValue mat with
                                            | (true, t) -> t
                                            | _ -> Unknown

                                        let texIds = 
                                            match info with
                                            | Unknown -> V3s(-1s, -1s, -1s)
                                            | All t | Cross t ->
                                                let id = getTextureId t
                                                V3s(int16 id, int16 id, int16 id)
                                            | BottomTop(bottom, top, side) ->
                                                let bid = getTextureId bottom
                                                let tid = getTextureId top
                                                let sid = getTextureId side
                                                V3s(int16 bid, int16 tid, int16 sid)
                                            

                                        yield { Offset = off + V3s(x,z,y); MaterialId = texIds }
                                i <- i + 1
                |]
            tree <- Octree.add chunk tree
            iter <- iter + 1
            if iter % 100 = 0 then
                printfn "chunk at %A: %d" off tree.Count
        )


        let atlas =
            use __ = env.Runtime.ContextLock
            let atlas = env.Runtime.CreateTexture2DArray(V2i(16, 16), TextureFormat.Rgba8, 4, 1, textureList.Count)
            for i in 0 .. textureList.Count - 1 do  
                try
                    let img = PixImageSharp.Create(textureList.[i]).ToPixImage<byte>(Col.Format.RGBA).SubImage(V2i.Zero, V2i(16,16))

                    if textureList.[i].ToLower().Contains "_leaves" then 
                        img.GetMatrix<C4b>().SetMap(img.GetMatrix<C4b>(), fun c -> C4b(0uy, c.G, 0uy, c.A)) |> ignore


                        
                    env.Runtime.Upload(atlas, img, level = 0, slice = i)
                    //let mutable s = img.Size
                    //let mutable img = img
                    //for l in 0 .. 3 do
                    //    env.Runtime.Upload(atlas, img, level = l, slice = i)
                    //    let s = img.Size/2
                    //    if s.AllGreater 0 then
                    //        let nimg = PixImage<byte>(Col.Format.RGBA, img.Size/2)
                    //        NativeVolume.using img.Volume (fun pSrc ->
                    //            NativeVolume.using nimg.Volume (fun pDst ->
                    //                NativeVolume.blit pSrc pDst
                    //            )
                    //        )
                    //        img <- nimg
                with _ ->
                    Log.error "nasdhbaiksdnjasndkjasndkjasd: %A" textureList.[i]
            env.Runtime.GenerateMipMaps atlas
            atlas
       
        let world = World.treeWorld env.Window atlas tree 1.75
        let center = world.Bounds.Center.XYZ// + world.Bounds.Max.OOZ
        let cam = { CameraController.initial with camera = CameraView.lookAt center (center + V3d.IOO) V3d.OOI }

        let (p1, floor) = 
            world.Hit (cam.camera.Location + V3d(0,0,1000)) cam.camera.Location

       //let initialTargets =
       //    HashMap.ofList [
       //        "target_0", {currentHp = 50; pos = V3d(0.0,0.0,5.0); radius = 0.5; maxHp = 50}
       //        "target_1", {currentHp = 150; pos = V3d(4.0,4.0,4.0); radius = 2.0; maxHp = 150}
       //    ]
        let random = System.Random() 
            
        let initialTargets = 
            HashMap.ofList [
                for i = 0 to 10 - 1 do
                    let randomHealth = random.Next(10,200)
                    let randomRadius = random.Next(0,10)
                    let randomPosition = V3d(random.Next(-50,50),random.Next(-50,50),random.Next(3,20))
                    let name = sprintf "target_%i" i
                    name,{currentHp = randomHealth; maxHp = randomHealth; pos = randomPosition ;radius = randomRadius }   
            ]
        
        {
            world = world
            onFloor = floor
            size = V2i.II
            camera = { cam with camera = cam.camera.WithLocation(p1) }
            proj = Frustum.perspective 90.0 0.1 1000.0 1.0
            time = 0.0
            targets = initialTargets
            moveSpeed = 25.5
            airAccel = 0.0012
        }

    let update (env : Environment<Message>) (model : Model) (message : Message) =

        let inline cam (msg : CameraMessage) (m : Model) =
            let p0 = m.camera.camera.Location
            let newCam = CameraController.update model.camera msg
            let p1 = newCam.camera.Location
            let newModel = 
                if p0 <> p1 then
                    let (p1, floor) = m.world.Hit p0 p1
                    let newCam = { newCam with camera = newCam.camera.WithLocation(p1) }
                    { m with camera = newCam; onFloor = floor }
                else    
                    { m with camera = newCam }
            if newModel.onFloor then   
                { newModel with 
                    camera = 
                        { newModel.camera with 
                            velocity = 
                                let vn = Fun.Lerp(0.05, newModel.camera.velocity, newModel.camera.move)
                                V3d(vn.X,vn.Y,newModel.camera.move.Z)
                        } 
                }
            else
                { newModel with 
                    camera = 
                        { newModel.camera with 
                            velocity =      
                                //let vn = Fun.Lerp(0.05, newModel.camera.velocity, newModel.camera.move)
                                
                                let x = model.airAccel * newModel.camera.move.X + newModel.camera.velocity.X
                                let y = model.airAccel * newModel.camera.move.Y + newModel.camera.velocity.Y
                                V3d(x,y,newModel.camera.velocity.Z)
                        } 
                }

        match message with
        | MouseMove delta -> model |> cam (CameraMessage.Look delta)
        | KeyDown Keys.W -> model |> cam (CameraMessage.StartMove (V3d(0.0, model.moveSpeed, 0.0)))
        | KeyUp Keys.W -> model |> cam (CameraMessage.StopMove (V3d(0.0, model.moveSpeed, 0.0)))
        | KeyDown Keys.S -> model |> cam (CameraMessage.StartMove (V3d(0.0, -model.moveSpeed, 0.0)))
        | KeyUp Keys.S -> model |> cam (CameraMessage.StopMove (V3d(0.0, -model.moveSpeed, 0.0)))
        | KeyDown Keys.A  -> model |> cam (CameraMessage.StartMove (V3d(-model.moveSpeed, 0.0, 0.0)))
        | KeyUp Keys.A -> model |> cam (CameraMessage.StopMove (V3d(-model.moveSpeed, 0.0, 0.0)))
        | KeyDown Keys.D -> model |> cam (CameraMessage.StartMove (V3d(model.moveSpeed, 0.0, 0.0)))
        | KeyUp Keys.D -> model |> cam (CameraMessage.StopMove (V3d(model.moveSpeed, 0.0, 0.0)))
        | KeyDown Keys.Space -> model |> cam (CameraMessage.StartMove (V3d(0.0, 0.0, 10.0)))
        | KeyUp Keys.Space -> model |> cam (CameraMessage.StopMove (V3d(0.0, 0.0, 10.0)))
        
        | KeyDown Keys.O -> 
            let n = model.moveSpeed + 0.1
            printfn "moveSpeed %.2f" n
            { model with moveSpeed = n }

        | KeyDown Keys.P -> 
            let n = model.moveSpeed - 0.1
            printfn "moveSpeed %.2f" n
            { model with moveSpeed = n }

        | KeyDown Keys.U -> 
            let n = model.airAccel + 0.00005
            printfn "airAccel %f" n
            { model with airAccel = n }

        | KeyDown Keys.I -> 
            let n = model.airAccel - 0.00005
            printfn "airAccel %f" n
            { model with airAccel = n }

        | Resize s -> 
            { model with 
                size = s
                proj = Frustum.perspective 90.0 0.1 1000.0 (float s.X / float s.Y) 
            }

        | UpdateTime(t, dt) ->
            let model = model |> cam (CameraMessage.UpdateTime(t, dt))
            if model.onFloor then
                { model with
                    time = t
                }
            else
                let cam = model.camera
                { model with
                    time = t
                    camera = 
                        { cam with 
                            velocity = cam.velocity - V3d(0.0, 0.0, 20.81) * dt
                        }
                }

        | KeyDown _ 
        | KeyUp _ 
        | MouseUp _ -> 
            model
        | MouseDown _ -> 
            let shotRay =
                let p = model.camera.camera.Location
                let d = model.camera.camera.Forward
                Ray3d(p, d)
            
            let hittedTarget =
                model.targets 
                |> HashMap.choose (fun name t -> 
                    let s = Sphere3d(t.pos, t.radius)
                    //let r = t.radius
                    //let d = model.camera.camera.Forward
                    //let x = (model.camera.camera.Location - t.pos)
                    //let b = 2.0*d*x
                    //let c = -r*r+x*x
                    //let rs = (-b + sqrt (b*b-4.0*c))/2.0
                    let d0 = t.pos - model.camera.camera.Location
                    let d1 = model.camera.camera.Forward
                    let inFront = (Vec.dot d0.Normalized d1.Normalized) > 0.0
                    let mutable tout = 0.0
                    let isHit = shotRay.HitsSphere(t.pos,t.radius,&tout)
                    //shotRay.Intersects(s) && inFront
                    match isHit && inFront with
                    |true -> Some tout
                    |false -> None
                ) 
                |> HashMap.toList
                |> List.sortBy snd
                |> List.tryHead
                |> Option.map fst

            let updatedTargets : HashMap<string, Target> =
                match hittedTarget with
                | None -> model.targets
                | Some hit -> 
                    model.targets
                    |> HashMap.alter hit (fun altV -> 
                        
                        
                        match altV with
                        |None -> None
                        |Some target -> 
                            let newHp = target.currentHp - 10
                            match newHp > 0 with
                            | true -> Some {target with currentHp = newHp}
                            | false -> None

                        
                    
                    )


                //|> HashMap.map (fun name t -> 
                  //  {t with currentHp = t.currentHp - 10}
                //)
            
            //let newTargets =
            //    HashMap.union model.targets hittedTarget
            //    |> HashMap.filter (fun n t -> 
            //        t.currentHp > 0
            //    )
            //
            {model with targets = updatedTargets}

    let view (env : Environment<Message>) (model : AdaptiveModel) =
        events env
        let worldSg =
            model.world.Scene env.Window

        let gunSg = Gun.scene env.Window
        let textSg = 
            Text.statusTextSg env.Window (model.camera.velocity |> AVal.map (fun v -> sprintf "%.2f" v.Length)) (AVal.constant true)


        let targetsSg =
            model.targets 
            |> AMap.toASet 
            |> ASet.map (fun (name,t) -> 
                Target.SceneGraph t name
                
               
            ) |> Sg.set
            
        Sg.ofList [worldSg; gunSg; textSg; targetsSg]
            |> Sg.viewTrafo (model.camera.camera |> AVal.map CameraView.viewTrafo)
            |> Sg.projTrafo (model.proj |> AVal.map Frustum.projTrafo)

