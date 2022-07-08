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

    let playerTrafo = cval (Trafo3d.FromBasis(-V3d.IOO, V3d.OOI, V3d.OIO, V3d(0,0,2)))
    let keyboard = win.Keyboard
    let velocity = 8.0

    win.Fullcreen <- true
    win.Cursor <- Cursor.None
    win.Mouse.Move.Values.Add(fun (o,n) ->
        transact (fun () ->
            let delta = n.Position - o.Position
            playerTrafo.Value <-
                Trafo3d.RotationY(float delta.X * -0.03) *
                playerTrafo.Value 
        )
    )

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

            let wDown = keyboard.IsDown Keys.W

            while true do
                let t = sw.Elapsed.TotalSeconds
                let dt = t - last
                last <- t

                if AVal.force wDown then
                    let fw = playerTrafo.Value.Forward.TransformDir(V3d(0,0,-1))
                    transact (fun () ->
                        playerTrafo.Value <-
                            playerTrafo.Value * Trafo3d.Translation (fw * velocity * dt)
                    )


                mm.Wait()


    let scene =
        Sg.ofList [
            Sg.fullScreenQuad
            |> Sg.scale 100.0
            |> Sg.diffuseTexture DefaultTextures.checkerboard
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }

            let cfg : TextConfig =
                {
                    align = TextAlignment.Center
                    color = C4b.Red
                    flipViewDependent = true
                    font = FontSquirrel.Hack.Regular
                    renderStyle = RenderStyle.NoBoundary
                }
            Sg.textWithConfig cfg (AVal.constant "AARDWARS")
            |> Sg.transform (Trafo3d.RotationX(Constant.PiHalf))
            |> Sg.translate 0.0 -10.0 3.0
        ]
        |> Sg.viewTrafo (playerTrafo |> AVal.map (fun t -> t.Inverse))
        |> Sg.projTrafo (win.Sizes |> AVal.map (fun s -> Frustum.perspective 60.0 0.1 100.0 (float s.X / float s.Y) |> Frustum.projTrafo))

    win.RenderTask <- 
        Sg.compile' win.Runtime win.FramebufferSignature false scene
    
    win.Focus()
    win.Run()
    
    0