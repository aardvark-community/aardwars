open System.Threading
open System.Reflection
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim
open Aardvark.Rendering.Text

[<EntryPoint>]
let main (_args : string[]) =
    Aardvark.Init()
    let app = new OpenGlApplication()
    let win = app.CreateGameWindow()

    let camera = cval (CameraView.lookAt (V3d(0,0,2)) (V3d(0,100,2)) V3d.OOI) 
    let keyboard = win.Keyboard
    let moveVelocity = 20.0


    let glfw = win.GetType().GetField("glfw", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> Silk.NET.GLFW.Glfw
    let hwin = win.GetType().GetField("win", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> nativeptr<Silk.NET.GLFW.WindowHandle>
    let mutable mouseDelta = V2d.Zero

    let mutable v = V3d.Zero
    //win.Fullcreen <- true
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

    let mutable moveVelocity = V3d.Zero
    let mutable velocity = V3d.Zero
    let mutable onFloor = true
    win.Keyboard.Down.Values.Add (fun k ->
        match k with
        | Keys.Escape -> win.Close()
        | Keys.W -> moveVelocity.Y <- moveVelocity.Y + 8.0
        | Keys.S -> moveVelocity.Y <- moveVelocity.Y - 8.0
        | Keys.Space -> moveVelocity.Z <- moveVelocity.Z + 10.0
        | Keys.A -> moveVelocity.X <- moveVelocity.X - 5.0
        | Keys.D -> moveVelocity.X <- moveVelocity.X + 5.0
            
        | _ -> ()
    )
    win.Keyboard.Up.Values.Add (fun k ->
        match k with
        | Keys.W -> moveVelocity.Y <- moveVelocity.Y - 8.0
        | Keys.S -> moveVelocity.Y <- moveVelocity.Y + 8.0
        | Keys.A -> moveVelocity.X <- moveVelocity.X + 5.0
        | Keys.D -> moveVelocity.X <- moveVelocity.X - 5.0
            
        | _ -> ()
    )
    
    
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let thread = 
        startThread <| fun () ->

            use mm = new MultimediaTimer.Trigger(5)

            let mutable lastTime = sw.Elapsed.TotalSeconds
            //let mutable lastMouse = AVal.force(win.Mouse.Position).Position
            let wDown = keyboard.IsDown Keys.W

            let move (delta : V3d) =
                transact (fun () ->
                    camera.Value <-
                        camera.Value.WithLocation(camera.Value.Location + delta)
                )

            while true do
                let t = sw.Elapsed.TotalSeconds
                let dt = t - lastTime
                lastTime <- t

                //let np = AVal.force(win.Mouse.Position).Position
                let delta = mouseDelta
                mouseDelta <- V2d.Zero

                // lookaround
                transact (fun () -> 
                    let fw = camera.Value.Forward
                    let r = camera.Value.Right
                    let trafo =
                        M44d.Rotation(r, float delta.Y * -0.005 ) *
                        M44d.Rotation(V3d.OOI, float delta.X * -0.005   )
                    let newForward = trafo.TransformDir fw |> Vec.normalize

                    let p = camera.Value.WithForward newForward
                    camera.Value <- p
                )   
                
                if onFloor then 
                    velocity <- moveVelocity
                else
                    moveVelocity.Z <- 0.0
                    let geschwindigkeit = velocity.Z - 9.8076 * dt
                    //let relativeBewegung = x + geschw * dt
                    //let geschwindigkeit = velocity.Z
                    velocity.Z <- geschwindigkeit


                let fw = camera.Value.Forward.XYO |> Vec.normalize
                let r = camera.Value.Right.XYO |> Vec.normalize
                move ((fw * velocity.Y + r * velocity.X + V3d.OOI * velocity.Z) * dt)
                onFloor <- camera.Value.Location.Z <= 2.0 
                

                

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

            for i in 1 .. 50 do
                let speed = rand.UniformDouble() * Constant.Pi
                Sg.box' (rand.UniformC3f().ToC4b()) (Box3d(-V3d.III, V3d.III))
                |> Sg.trafo (win.Time |> AVal.map (fun _ -> Trafo3d.RotationZ(speed * sw.Elapsed.TotalSeconds)))
                |> Sg.translation' (V3d(rand.UniformV2d(Box2d(-50.0, -50.0, 50.0, 50.0)), 1.0))
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.simpleLighting
                }

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
        |> Sg.projTrafo (win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo))

    win.RenderTask <- 
        Sg.compile' win.Runtime win.FramebufferSignature false scene
    
    win.Focus()
    win.Run()
    
    0