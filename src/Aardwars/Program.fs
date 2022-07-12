open System.Threading
open System.Reflection
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Rendering.Text

let statusTextSg (win : IRenderWindow) (t : aval<string>) (showText : aval<bool>) =
    let textProj =
        win.Sizes |> AVal.map (fun s ->
            Trafo3d.Scale(float s.Y / float s.X, 1.0, 1.0)
        )
                        
    let font = FontSquirrel.Anonymous_Pro.Regular
    let leftTextSg =
        let shape = 
            t |> AVal.map (fun t -> font.Layout(C4b.White, t))

        let trafo = 
            (win.Sizes, shape) ||> AVal.map2 (fun s shape ->
                let scale = 18.0 / float s.Y * 2.0
                let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
                let minX = -float s.X / float s.Y
                let x = minX - bounds.Min.X + 0.02
                let y = 1.0 - bounds.Max.Y - 0.02

                Trafo3d.Scale(scale) *
                Trafo3d.Translation(x, y, -1.0)
            )

        Sg.shape shape
        |> Sg.trafo trafo
    //let rightTextSg =

    //    let shape = 
    //        statsText |> AVal.map (fun t -> font.Layout(C4b.White, t))

    //    let trafo = 
    //        (win.Sizes, shape) ||> AVal.map2 (fun s shape ->
    //            let scale = 18.0 / float s.Y * 2.0
    //            let bounds = Box2d(shape.bounds.Min * scale, shape.bounds.Max * scale)
    //            let maxX = float s.X / float s.Y
    //            let x = maxX - bounds.Max.X - 0.02
    //            let y = 1.0 - bounds.Max.Y - 0.02
    //            Trafo3d.Scale(scale) *
    //            Trafo3d.Translation(x, y, -1.0)
    //        )

    //    Sg.shape shape
    //    |> Sg.trafo trafo
    Sg.ofList [leftTextSg]
    |> Sg.onOff showText
    |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
    |> Sg.projTrafo textProj
    |> Sg.depthTest' DepthTest.None
    |> Sg.blendMode' BlendMode.Blend

let repairMotion (p0 : V3d) (p1 : V3d) (b : Box3d) (r : float) =
    let d1 = 
        if b.Contains p1 then 0.0
        else b.GetMinimalDistanceTo p1

    if d1 > r then
        None
    else
        let dir = p1 - p0
        let (bMin, _) = b.GetMinMaxInDirection(dir)


        let pt = p1.GetClosestPointOn(b)
        let d = Vec.distance pt p1
        if d <= r then
            let e = 1E-1
            let x = 
                if Fun.ApproximateEquals(pt.X, b.Max.X,e) then 1
                elif Fun.ApproximateEquals(pt.X, b.Min.X,e) then -1
                else 0
            let y = 
                if Fun.ApproximateEquals(pt.Y, b.Max.Y,e) then 1
                elif Fun.ApproximateEquals(pt.Y, b.Min.Y,e) then -1
                else 0
            let z = 
                if Fun.ApproximateEquals(pt.Z, b.Max.Z,e) then 1
                elif Fun.ApproximateEquals(pt.Z, b.Min.Z,e) then -1
                else 0

            let inline repairX() =
                if x > 0 then Some (false, V3d(b.Max.X + r, p1.Y, p1.Z))
                else Some (false, V3d(b.Min.X - r, p1.Y, p1.Z))
                
            let inline repairY() =
                if y > 0 then Some (false,V3d(p1.X, b.Max.Y + r, p1.Z))
                else Some (false, V3d(p1.X, b.Min.Y - r, p1.Z))
                
            let inline repairZ() =
                if z > 0 then Some (true, V3d(p1.X, p1.Y, b.Max.Z + r))
                else Some (false, V3d(p1.X, p1.Y, b.Min.Z - r))


            if x <> 0 && y = 0 && z = 0 then repairX()
            elif x = 0 && y <> 0 && z = 0 then repairY()
            elif x = 0 && y = 0 && z <> 0 then repairZ()

            elif x <> 0 && y <> 0 && z = 0 then
                if (p0.XY - pt.XY).Abs().MajorDim = 0 then repairX()
                else repairY()
                    
            elif x <> 0 && y = 0 && z <> 0 then
                if (p0.XZ - pt.XZ).Abs().MajorDim = 0 then repairX()
                else repairZ()
                
            elif x = 0 && y <> 0 && z <> 0 then
                if (p0.YZ - pt.YZ).Abs().MajorDim = 0 then repairY()
                else repairZ()

            else
                match (p0 - pt).Abs().MajorDim with
                | 0 -> repairX()
                | 1 -> repairY()
                | _ -> repairZ()

        else
            None

[<EntryPoint>]
let main (_args : string[]) =
    Aardvark.Init()
    let app = new OpenGlApplication()
    let win = app.CreateGameWindow()

    let camera = cval (CameraView.lookAt (V3d(0,0,50)) (V3d(0,100,2)) V3d.OOI) 
    let keyboard = win.Keyboard
    let movespeed = 5.0
    let gravity = 1.725
    let jumpHeight = 6.95 
    let airAccel = 0.025
    let mutable playerHeight = 1.35

    let glfw = win.GetType().GetField("glfw", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> Silk.NET.GLFW.Glfw
    let hwin = win.GetType().GetField("win", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> nativeptr<Silk.NET.GLFW.WindowHandle>
    let mutable mouseDelta = V2d.Zero

    let mutable v = V3d.Zero
    win.Fullcreen <- false
    win.Cursor <- Cursor.None
    win.VSync <- true
    
    win.Mouse.Move.Values.Add(fun (o,n) ->
        let c = V2d (AVal.force win.Sizes) / 2.0
        let mutable px = 0.0
        let mutable py = 0.0
        glfw.GetCursorPos(hwin, &px, &py)
        glfw.SetCursorPos(hwin, c.X, c.Y)
        mouseDelta <- mouseDelta + V2d(px, py) - c
    )
    let mutable sprint = 10.0
    let mutable moveVelocity = V3d.Zero
    let mutable velocity = V3d.Zero
    let mutable onFloor = true
     
    win.Keyboard.DownWithRepeats.Values.Add (fun k -> 
        match k with 
        | Keys.Space -> moveVelocity.Z <- jumpHeight
        | _ -> ()
    )

    win.Keyboard.Down.Values.Add (fun k ->
        match k with
        | Keys.Escape -> win.Close()
        | Keys.W -> moveVelocity.Y <- moveVelocity.Y + movespeed
        | Keys.S -> moveVelocity.Y <- moveVelocity.Y - movespeed
        | Keys.A -> moveVelocity.X <- moveVelocity.X - 0.9 * movespeed
        | Keys.D -> moveVelocity.X <- moveVelocity.X + 0.9 * movespeed
        | Keys.RightShift -> playerHeight <- playerHeight - 0.25
        | _ -> ()
    )
    win.Keyboard.Up.Values.Add (fun k ->
        match k with
        | Keys.W -> moveVelocity.Y <- moveVelocity.Y - movespeed
        | Keys.S -> moveVelocity.Y <- moveVelocity.Y + movespeed
        | Keys.A -> moveVelocity.X <- moveVelocity.X + 0.9 * movespeed
        | Keys.D -> moveVelocity.X <- moveVelocity.X - 0.9 * movespeed
        | Keys.Space -> moveVelocity.Z <- 0.0
        | Keys.RightShift -> playerHeight <- playerHeight + 0.25
        | _ -> ()
    )
    
    let testScene(gridSize : V2i) (blockCount : int) =
        let rand = RandomSystem()
        let grid = Array2D.create gridSize.X gridSize.Y 1
        let offset = V2d gridSize / 2.0
        for i in 0 .. blockCount - 1 do
            let x = int <| clamp 0.0 (float gridSize.X - 1.0) (round <| rand.Gaussian(float gridSize.X/2.0, float gridSize.X/4.0))
            let y = int <| clamp 0.0 (float gridSize.Y - 1.0) (round <| rand.Gaussian(float gridSize.Y/2.0, float gridSize.Y/4.0))

            grid.[x,y] <- grid.[x,y] + 1
        [|
            for x in 0 .. gridSize.X - 1 do
                for y in 0 .. gridSize.Y - 1 do
                    let cnt = grid.[x,y]
                    for z in 0 .. cnt - 1 do
                        Trafo3d.Scale(1.01) *
                        Trafo3d.Translation(float x, float y, float z) *
                        Trafo3d.Translation(V3d(-offset, 0.0))
        |]
    Log.startTimed "Create lvl"
    let trafos = testScene (V2i(100, 100)) 2000
    let worldBoxes = 
        trafos |> Array.map (fun t -> 
            let b = Box3d.Unit.Transformed(t)
            //Box3d.FromCenterAndSize(b.Center, b.Size + V3d.III * 0.01)
            b
        )
    Log.stop()
    
    let displayVelo = cval V3d.Zero
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let thread = 
        startThread <| fun () ->

            use mm = new MultimediaTimer.Trigger(5)

            let mutable lastTime = sw.Elapsed.TotalSeconds
            //let mutable lastMouse = AVal.force(win.Mouse.Position).Position
            let wDown = keyboard.IsDown Keys.W

            let moveTo (pos : V3d) =
                transact (fun () ->
                    camera.Value <-
                        camera.Value.WithLocation(pos)
                )
            while true do
                let t = sw.Elapsed.TotalSeconds
                let dt = t - lastTime
                lastTime <- t
                if dt < 20.0 then 
                    //let np = AVal.force(win.Mouse.Position).Position
                    let delta = mouseDelta
                    mouseDelta <- V2d.Zero

                    // lookaround
                    transact (fun () -> 
                        let fw = camera.Value.Forward
                        let r = camera.Value.Right
                    
                        let dy = 
                            if Vec.dot camera.Value.Sky fw > 0.991 then max delta.Y 0.0
                            elif Vec.dot camera.Value.Sky fw < -0.991 then min delta.Y 0.0
                            else delta.Y
                        let trafo =
                            M44d.Rotation(r, float dy * -0.005 ) *
                            M44d.Rotation(V3d.OOI, float delta.X * -0.005   )
                        let newForward = trafo.TransformDir fw |> Vec.normalize
                    
                        let p = camera.Value.WithForward newForward
                        camera.Value <- p
                    )   
                
                    if onFloor then 
                        //inertia on ground
                        let velo = Fun.Lerp(0.12, velocity.XY, moveVelocity.XY)
                        velocity <- V3d(velo.X, velo.Y, moveVelocity.Z)
                    else
                        //gravity
                        let geschwindigkeit = velocity.Z - gravity * 9.8076 * dt 
                        velocity.Z <- geschwindigkeit
                        //air control
                        velocity.XY <- velocity.XY + moveVelocity.XY.Normalized * airAccel

                    let fw = camera.Value.Forward.XYO |> Vec.normalize
                    let r = camera.Value.Right.XYO |> Vec.normalize
                    //position change in timestep dt
                    let delta = ((fw * velocity.Y + r * velocity.X + V3d.OOI * velocity.Z) * dt)
                    if not (delta |> Fun.IsTiny) then 
                        //collision with box/floor detection and repair position
                        let p0 = camera.Value.Location - (V3d.OOI * playerHeight)
                        let mutable p1 = (camera.Value.Location + delta) - (V3d.OOI * playerHeight)
                        let mutable zUp = false
                        let tryRepair p1 =
                            worldBoxes 
                            |> Array.fold (fun p1 b ->
                                let pos = repairMotion p0 p1 b 0.2
                                match pos with
                                | Some (z, p) -> 
                                    if z then zUp <- true
                                    p
                                | None -> p1
                            ) p1
                        let p1 = tryRepair p1

                        moveTo (p1 + (V3d.OOI * playerHeight))

                        if zUp then onFloor <- true
                        else onFloor <- camera.Value.Location.Z <= playerHeight //|| (contained = On Box.Flags.MaxZ)
                
                    transact (fun _ -> displayVelo.Value <- velocity)
                    mm.Wait()

    let cfg : TextConfig =
        {
            align = TextAlignment.Center
            color = C4b.Red
            flipViewDependent = true
            font = FontSquirrel.Hack.Regular
            renderStyle = RenderStyle.NoBoundary
        }

    let realPlayerTrafo =
        camera |> AVal.map (CameraView.viewTrafo)

    let scene =
        let rand = RandomSystem()
        Sg.ofList [
            Sg.fullScreenQuad
            |> Sg.scale 100.0
            |> Sg.diffuseTexture DefaultTextures.checkerboard
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }

            let box = Box3d.FromCenterAndSize(V3d.Zero, V3d(1000.0, 1000.0, 5.0))
            Sg.box' C4b.Red Box3d.Unit
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.instanced (AVal.constant trafos)

            //for i in 1 .. 50 do
            //    let speed = rand.UniformDouble() * Constant.Pi
            //    Sg.box' (rand.UniformC3f().ToC4b()) (Box3d(-V3d.III, V3d.III))
            //    |> Sg.trafo (win.Time |> AVal.map (fun _ -> Trafo3d.RotationZ(speed * sw.Elapsed.TotalSeconds)))
            //    |> Sg.translation' (V3d(rand.UniformV2d(Box2d(-50.0, -50.0, 50.0, 50.0)), 1.0))
            //    |> Sg.shader {
            //        do! DefaultSurfaces.trafo
            //        do! DefaultSurfaces.simpleLighting
            //    }
            Sg.textWithConfig cfg (AVal.constant "AARDWARS")
            |> Sg.transform (Trafo3d.RotationX(Constant.PiHalf))
            |> Sg.translate 0.0 -10.0 3.0

            let shape =
                let t = 3.0
                let r = 30.0
                let w = 10.0
                let t2 = 4.0
                let color = C4b.Red //C4b(C4b.Red)
                ShapeList.ofListWithRenderStyle RenderStyle.NoBoundary [
                    ConcreteShape.circle color t (Circle2d(V2d.Zero, r))
                    ConcreteShape.fillRectangle color (Box2d(r, -t/2.0, r + w, t/2.0))
                    ConcreteShape.fillRectangle color (Box2d(-r-w, -t/2.0, -r, t/2.0))
                    ConcreteShape.fillRectangle color (Box2d(-t/2.0, r, t/2.0, r + w))
                    ConcreteShape.fillRectangle color (Box2d(-t/2.0, -r - w, t/2.0, -r))
                    ConcreteShape.fillRectangle color (Box2d(-t2/2.0,-r,t2/2.0,r))
                    ConcreteShape.fillRectangle color (Box2d(-r,-t2/2.0,r,t2/2.0))
                ]
            Sg.shape (
                AVal.constant (
                    shape    
                )
            )
            |> Sg.viewTrafo' Trafo3d.Identity
            |> Sg.projTrafo (win.Sizes |> AVal.map (fun s -> Trafo3d.Scale(1.0 / float s.X, 1.0 / float s.Y, 1.0)))
        ]
        |> Sg.viewTrafo realPlayerTrafo
        |> Sg.projTrafo (win.Sizes |> AVal.map (fun s -> Frustum.perspective 90.0 0.2 10000.0 (float s.X / float s.Y) |> Frustum.projTrafo))

    let statusText =
        displayVelo |> AVal.map (fun v -> sprintf "%.2f" v.Length)
    let textSg = statusTextSg win statusText (AVal.constant true)
    let finalSg =
        Sg.ofList [scene; textSg]
    win.RenderTask <- 
        Sg.compile' win.Runtime win.FramebufferSignature false finalSg
    
    win.Focus()
    win.Run()
    
    0