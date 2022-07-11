open System.Threading
open System.Reflection
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Rendering.Text

type Containedness =
    | Outside
    | On of Box.Flags
    | Inside

let getContainedness (p : V3d) (b : Box3d) (eps : float) : Containedness = 
    let bigger = Box3d.FromCenterAndSize(b.Center, b.Size + V3d.III * eps)
    let smaller = Box3d.FromCenterAndSize(b.Center, b.Size - V3d.III * eps)
    match smaller.Contains(p), bigger.Contains(p) with
    | false, false -> Outside
    | false, true -> 
        let flag = 
            let pp = (p - b.Center)
            let m = pp.MajorDim
            if abs pp.Z + eps >= abs pp.X && abs pp.Z + eps >= abs pp.Y then 
                if pp.Z <= 0 then Box.Flags.MinZ else Box.Flags.MaxZ
            elif m = 0 then if pp.X <= 0 then Box.Flags.MinX else Box.Flags.MaxX
            elif m = 1 then if pp.Y <= 0 then Box.Flags.MinY else Box.Flags.MaxY
            elif m = 2 then if pp.Z <= 0 then Box.Flags.MinZ else Box.Flags.MaxZ
            else failwith "can not happen"
        On flag
    | true, true -> Inside
    | true, false -> failwith "can never happen"

let updatePosition (p0 : V3d) (p1 : V3d) (b : Box3d) (eps : float) = 
    let clampToFace face (p1 : V3d) =
        match face with 
        | Box.Flags.MinX -> 
            V3d(b.Min.X, p1.Y, p1.Z), On face
        | Box.Flags.MaxX -> 
            V3d(b.Max.X, p1.Y, p1.Z), On face
        | Box.Flags.MinY -> 
            V3d(p1.X, b.Min.Y, p1.Z), On face
        | Box.Flags.MaxY -> 
            V3d(p1.X, b.Max.Y, p1.Z), On face
        | Box.Flags.MinZ -> 
            V3d(p1.X, p1.Y, b.Min.Z), On face
        | Box.Flags.MaxZ -> 
            V3d(p1.X, p1.Y, b.Max.Z), On face
        | _ -> failwith "no face"
    match getContainedness p0 b eps, getContainedness p1 b eps with 

    | On _, On face -> 
        //printfn "ON %A" face
        clampToFace face p1
    | On face, Inside -> 
        //printfn "OI %A" face
        clampToFace face p1
    | Outside, (Inside | On _) -> 
        //let b = Box3d.FromCenterAndSize(b.Center, b.Size * 1.2)
        let d = p1 - p0
        let ray = Ray3d(p0,d)
        let vMin, _ = b.GetMinMaxInDirection(d)
        let px = Plane3d(V3d.IOO, vMin.X) 
        let py = Plane3d(V3d.OIO, vMin.Y) 
        let pz = Plane3d(V3d.OOI, vMin.Z) 
        let hitPlane (dim : int) =
            let p = if dim = 0 then px elif dim = 1 then py else pz
            let mutable pt = 0.0
            if ray.Intersects(p,&pt) then 
                let phit = ray.GetPointOnRay pt
                let containedInOther =
                    if dim = 0 then
                        b.Min.YZ.AllSmallerOrEqual(phit.YZ) &&
                        b.Max.YZ.AllGreaterOrEqual(phit.YZ)
                    elif dim = 1 then 
                        b.Min.XZ.AllSmallerOrEqual(phit.XZ) &&
                        b.Max.XZ.AllGreaterOrEqual(phit.XZ)
                    else 
                        b.Min.XY.AllSmallerOrEqual(phit.XY) &&
                        b.Max.XY.AllGreaterOrEqual(phit.XY)
                containedInOther
            else 
                false
        
        if hitPlane 0 then 
            let face = if vMin.X = b.Min.X then Box.Flags.MinX else Box.Flags.MaxX
            let newPos = V3d(vMin.X, p1.Y, p1.Z)
            printfn "intersect %A -> %A (%A)" p1 newPos face
            newPos, On face
        elif hitPlane 1 then 
            let face = if vMin.Y = b.Min.Y then Box.Flags.MinY else Box.Flags.MaxY
            let newPos = V3d(p1.X, vMin.Y, p1.Z)
            printfn "intersect %A -> %A (%A)" p1 newPos face
            newPos, On face
        else
            let face = if vMin.Z = b.Min.Z then Box.Flags.MinZ else Box.Flags.MaxZ
            let newPos = V3d(p1.X, p1.Y, vMin.Z)
            printfn "intersect %A -> %A (%A)" p1 newPos face
            newPos, On face
    | _, Outside -> p1, Outside
    | Inside, _ -> failwith "can never happen"
 

let repairMotion (p0 : V3d) (p1 : V3d) (b : Box3d) (r : float) =
    let d1 = 
        if b.Contains p1 then 0.0
        else b.GetMinimalDistanceTo p1

    if d1 > r then
        None
    else
        // b.GetMinimalDistanceTo p0 >= r
        let dir = p1 - p0
        let (bMin, _) = b.GetMinMaxInDirection(dir)


        let pt = p1.GetClosestPointOn(b)
        let d = Vec.distance pt p1
        if d <= r then

            let x = 
                if Fun.ApproximateEquals(pt.X, b.Max.X) then 1
                elif Fun.ApproximateEquals(pt.X, b.Min.X) then -1
                else 0
            let y = 
                if Fun.ApproximateEquals(pt.Y, b.Max.Y) then 1
                elif Fun.ApproximateEquals(pt.Y, b.Min.Y) then -1
                else 0
            let z = 
                if Fun.ApproximateEquals(pt.Z, b.Max.Z) then 1
                elif Fun.ApproximateEquals(pt.Z, b.Min.Z) then -1
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
                if (p0.XY - pt.XY).MajorDim = 0 then repairX()
                else repairY()
                    
            elif x <> 0 && y = 0 && z <> 0 then
                if (p0.XZ - pt.XZ).MajorDim = 0 then repairX()
                else repairZ()
                
            elif x = 0 && y <> 0 && z <> 0 then
                if (p0.YZ - pt.YZ).MajorDim = 0 then repairY()
                else repairZ()

            else
                match (p0 - pt).MajorDim with
                | 0 -> repairX()
                | 1 -> repairY()
                | _ -> repairZ()

        else
            None

        //let minX = Fun.ApproximateEquals(pt.X, b.Min.X)



        //let px = Plane3d(V3d.IOO, bMin.X)
        //let py = Plane3d(V3d.OIO, bMin.Y)
        //let pz = Plane3d(V3d.OOI, bMin.Z)

        //let hx = px.Height(p1)
        //let hy = py.Height(p1)
        //let hz = pz.Height(p1)

        //let xc = abs hx < r
        //let yc = abs hy < r
        //let zc = abs hz < r

        //match xc,yc,zc with
        //| true, false, false -> 
        //    let res =
        //        if hx < r then p1 + px.Normal * (r - hx)
        //        else p1 + px.Normal * (hx - r)
        //    Some res

        //| false, true, false -> 
        //    let res =
        //        if hy < r then p1 + py.Normal * (r - hy)
        //        else p1 + py.Normal * (hy - r)
        //    Some res

        //| false, false, true -> 
        //    let res =
        //        if hz < r then p1 + pz.Normal * (r - hz)
        //        else p1 + pz.Normal * (hz - r)
        //    Some res

        //| true, true, false  -> 
        //    let h0 = px.Height p0 |> abs
        //    let h1 = py.Height p0 |> abs
        //    if h0 > h1 then
        //        let res =
        //            if hx < r then p1 + px.Normal * (r - hx)
        //            else p1 + px.Normal * (hx - r)
        //        Some res
        //    else 
        //        let res =
        //            if hy < r then p1 + py.Normal * (r - hy)
        //            else p1 + py.Normal * (hy - r)
        //        Some res
                
        //| true, false, true  -> 
        //    let h0 = py.Height p0 |> abs
        //    let h1 = pz.Height p0 |> abs
        //    if h0 > h1 then
        //        let res =
        //            if hy < r then p1 + py.Normal * (r - hy)
        //            else p1 + py.Normal * (hy - r)
        //        Some res
        //    else 
        //        let res =
        //            if hz < r then p1 + pz.Normal * (r - hz)
        //            else p1 + pz.Normal * (hz - r)
        //        Some res
                
        //| false, true, true  ->
        //    let h0 = px.Height p0 |> abs
        //    let h1 = pz.Height p0 |> abs
        //    if h0 > h1 then
        //        let res =
        //            if hx < r then p1 + px.Normal * (r - hx)
        //            else p1 + px.Normal * (hx - r)
        //        Some res
        //    else 
        //        let res =
        //            if hz < r then p1 + pz.Normal * (r - hz)
        //            else p1 + pz.Normal * (hz - r)
        //        Some res
        //| true, true, true   -> failwith "corner"
        //| false,false, false -> failwith "can not happen"







[<EntryPoint>]
let main (_args : string[]) =
    Aardvark.Init()
    let app = new OpenGlApplication()
    let win = app.CreateGameWindow()

    let camera = cval (CameraView.lookAt (V3d(0,0,50)) (V3d(0,100,2)) V3d.OOI) 
    let keyboard = win.Keyboard
    let movespeed = 15.0
    let gravity = 1.5
    let jumpspeed = 0.8
    let playerHeight = 1.85


    let glfw = win.GetType().GetField("glfw", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> Silk.NET.GLFW.Glfw
    let hwin = win.GetType().GetField("win", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> nativeptr<Silk.NET.GLFW.WindowHandle>
    let mutable mouseDelta = V2d.Zero

    let mutable v = V3d.Zero
    win.Fullcreen <- true
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
     
    win.Keyboard.Down.Values.Add (fun k ->
        match k with
        | Keys.Escape -> win.Close()
        | Keys.W -> moveVelocity.Y <- moveVelocity.Y + movespeed
        | Keys.S -> moveVelocity.Y <- moveVelocity.Y - movespeed
        | Keys.Space -> moveVelocity.Z <- moveVelocity.Z + movespeed * jumpspeed
        | Keys.A -> moveVelocity.X <- moveVelocity.X - 0.9 * movespeed
        | Keys.D -> moveVelocity.X <- moveVelocity.X + 0.9 * movespeed
        //| Keys.LeftShift -> moveVelocity.XY <- moveVelocity.XY + 5.0
        
             
        | _ -> ()
    )
    win.Keyboard.Up.Values.Add (fun k ->
        match k with
        | Keys.W -> moveVelocity.Y <- moveVelocity.Y - movespeed
        | Keys.S -> moveVelocity.Y <- moveVelocity.Y + movespeed
        | Keys.A -> moveVelocity.X <- moveVelocity.X + 0.9 * movespeed
        | Keys.D -> moveVelocity.X <- moveVelocity.X - 0.9 * movespeed
        
            
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
    let trafos = testScene (V2i(100, 100)) 1000
    let worldBoxes = 
        trafos |> Array.map (fun t -> 
            let b = Box3d.Unit.Transformed(t)
            //Box3d.FromCenterAndSize(b.Center, b.Size + V3d.III * 0.01)
            b
        )
    
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
                        velocity <- moveVelocity
                    else
                        moveVelocity.Z <- 0.0
                        let geschwindigkeit = velocity.Z - gravity * 9.8076 * dt
                        //let relativeBewegung = x + geschw * dt
                        //let geschwindigkeit = velocity.Z
                        velocity.Z <- geschwindigkeit

                    let fw = camera.Value.Forward.XYO |> Vec.normalize
                    let r = camera.Value.Right.XYO |> Vec.normalize
                    let delta = ((fw * velocity.Y + r * velocity.X + V3d.OOI * velocity.Z) * dt)
                    if not (delta |> Fun.IsTiny) then 
                        let p0 = camera.Value.Location - (V3d.OOI * playerHeight)
                        let mutable p1 = (camera.Value.Location + delta) - (V3d.OOI * playerHeight)
                        let mutable zUp = false
                        let (p1) =
                            let res = 
                                worldBoxes 
                                |> Array.fold (fun p1 b ->
                                    let pos = repairMotion p0 p1 b 0.2
                                    match pos with
                                    | Some (z, p) -> 
                                        if z then zUp <- true
                                        p
                                    | None -> p1
                                ) p1
                            res

                        moveTo (p1 + (V3d.OOI * playerHeight))

                        if zUp then onFloor <- true
                        else
                            onFloor <- camera.Value.Location.Z <= playerHeight + 0.01 //|| (contained = On Box.Flags.MaxZ)
                
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

    win.RenderTask <- 
        Sg.compile' win.Runtime win.FramebufferSignature false scene
    
    win.Focus()
    win.Run()
    
    0