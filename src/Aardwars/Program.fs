open System.Threading
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
    let moveVelocity = 8.0

    //win.Fullcreen <- true
    win.Cursor <- Cursor.None
    win.VSync <- true

    win.Keyboard.Down.Values.Add (fun k ->
        match k with
        | Keys.Escape -> win.Close()
        | _ -> ()
    )

    let thread = 
        startThread <| fun () ->

            use mm = new MultimediaTimer.Trigger(4)
            let sw = System.Diagnostics.Stopwatch.StartNew()
            let mutable last = sw.Elapsed.TotalSeconds
            let mutable lastMouse = AVal.force(win.Mouse.Position).Position
            let wDown = keyboard.IsDown Keys.W

            let move (delta : V3d) =
                transact (fun () ->
                    camera.Value <-
                        camera.Value.WithLocation(camera.Value.Location + delta)
                )

            while true do
                let t = sw.Elapsed.TotalSeconds
                let dt = t - last
                last <- t

                let np = AVal.force(win.Mouse.Position).Position
                let delta = np - lastMouse
                lastMouse <- np

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

                // go forward with w
                if AVal.force wDown then
                    let fw = camera.Value.Forward.XYO |> Vec.normalize
                    move (fw * moveVelocity * dt)


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
        Sg.ofList [
            Sg.fullScreenQuad
            |> Sg.scale 100.0
            |> Sg.diffuseTexture DefaultTextures.checkerboard
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }

            Sg.textWithConfig cfg (AVal.constant "AARDWARS")
            |> Sg.transform (Trafo3d.RotationX(Constant.PiHalf))
            |> Sg.translate 0.0 -10.0 3.0
        ]
        |> Sg.viewTrafo realPlayerTrafo
        |> Sg.projTrafo (win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo))

    win.RenderTask <- 
        Sg.compile' win.Runtime win.FramebufferSignature false scene
    
    win.Focus()
    win.Run()
    
    0