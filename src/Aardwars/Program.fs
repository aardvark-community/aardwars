open System.Threading
open System.Reflection
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Rendering.Text

open Aardvars

[<EntryPoint>]
let main (_args : string[]) =
    
    Aardvark.Init()
    let app = new OpenGlApplication()
    let win = app.CreateGameWindow()

    let camera = cval (CameraView.lookAt (V3d(0,0,5)) (V3d(0,100,2)) V3d.OOI) 
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

    let worldBoxes, trafos = Scene.createTestScene
    
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
                                let pos = Motion.repairMotion p0 p1 b 0.2
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

    let gunProjection =
        (win.Sizes |> AVal.map (fun s -> Frustum.perspective 110.0 0.0001 20.0 (float s.X / float s.Y) |> Frustum.projTrafo))

    let secondFbo =
        let sigg =   
            win.Runtime.CreateFramebufferSignature([
                DefaultSemantic.Colors, TextureFormat.Rgba8; 
                DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
               ]
            )
        let task = 
            Import.importGun()
                |> Sg.transform 
                    (
                        Trafo3d.Scale(1.0,1.0,-1.0) *
                        Trafo3d.Scale(0.5) *
                        Trafo3d.Translation(1.0,-1.0,-1.0)
                    )
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.diffuseTexture
                    do! DefaultSurfaces.simpleLighting
                }
                |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
                |> Sg.projTrafo gunProjection
                //|> Sg.depthTest' DepthTest.None
                |> Sg.cullMode' CullMode.None
                |> Sg.fillMode' FillMode.Fill
                |> Sg.compile app.Runtime sigg
                
        let c : ClearValues =
            clear {
                color (C4b(0uy,0uy,0uy,0uy))
                depth 1.0
                stencil 0
            }

        let t = RenderTask.renderToColorWithClear win.Sizes c task 
        t

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

            //Scene.boxSg trafos

            Sg.textWithConfig cfg (AVal.constant "AARDWARS")
            |> Sg.transform (Trafo3d.RotationX(Constant.PiHalf))
            |> Sg.translate 0.0 -10.0 3.0

            let shape =
                let t = 3.0
                let r = 30.0
                let w = 10.0
                let t2 = 4.0
                let color = C4b.Red 
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

    let statusText = displayVelo |> AVal.map (fun v -> sprintf "%.2f" v.Length)
    let textSg = Text.statusTextSg win statusText (AVal.constant true)
    let secondPassSg = 
        Sg.fullScreenQuad
        |> Sg.diffuseTexture secondFbo
        |> Sg.shader {
            do! DefaultSurfaces.diffuseTexture
        }
        |> Sg.blendMode' BlendMode.Blend
        |> Sg.pass (RenderPass.after "§iasfj" RenderPassOrder.Arbitrary RenderPass.main)

    let finalSg =
        Sg.ofList [scene; textSg; secondPassSg]
    win.RenderTask <- 
        Sg.compile' win.Runtime win.FramebufferSignature false finalSg
    
    win.Focus()
    win.Run()
    
    0