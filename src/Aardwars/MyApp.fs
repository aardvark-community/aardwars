namespace Elm

open Adaptify
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open FSharp.Data.Adaptive
open Aardvark.Application
open Elm
open System.Reflection

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
    let moveSpeed = 20.0

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



    let intitial (env : Environment<Message>) = 

        let world = PlaneWorld(2.0) :> World
        let cam = CameraController.initial

        let (p1, floor) = world.Hit(cam.camera.Location + V3d(0,0,1000), cam.camera.Location)

        {
            world = world
            onFloor = floor
            size = V2i.II
            camera = { cam with camera = cam.camera.WithLocation(p1) }
            proj = Frustum.perspective 90.0 0.1 1000.0 1.0
            time = 0.0
        }

    let update (env : Environment<Message>) (model : Model) (message : Message) =

        let inline cam (msg : CameraMessage) (m : Model) =
            let p0 = m.camera.camera.Location
            let newCam = CameraController.update model.camera msg
            let p1 = newCam.camera.Location
            let newModel = 
                if p0 <> p1 then
                    let (p1, floor) = m.world.Hit(p0, p1)
                    let newCam = { newCam with camera = newCam.camera.WithLocation(p1) }
                    { m with camera = newCam; onFloor = floor }
                else    
                    { m with camera = newCam }
            if newModel.onFloor then   
                { newModel with camera = { newModel.camera with velocity = newModel.camera.move } }
            else
                newModel

        match message with

        | MouseMove delta -> model |> cam (CameraMessage.Look delta)
        | KeyDown Keys.W -> model |> cam (CameraMessage.StartMove (V3d(0.0, moveSpeed, 0.0)))
        | KeyUp Keys.W -> model |> cam (CameraMessage.StopMove (V3d(0.0, moveSpeed, 0.0)))
        | KeyDown Keys.S -> model |> cam (CameraMessage.StartMove (V3d(0.0, -moveSpeed, 0.0)))
        | KeyUp Keys.S -> model |> cam (CameraMessage.StopMove (V3d(0.0, -moveSpeed, 0.0)))
        | KeyDown Keys.A  -> model |> cam (CameraMessage.StartMove (V3d(-moveSpeed, 0.0, 0.0)))
        | KeyUp Keys.A -> model |> cam (CameraMessage.StopMove (V3d(-moveSpeed, 0.0, 0.0)))
        | KeyDown Keys.D -> model |> cam (CameraMessage.StartMove (V3d(moveSpeed, 0.0, 0.0)))
        | KeyUp Keys.D -> model |> cam (CameraMessage.StopMove (V3d(moveSpeed, 0.0, 0.0)))
        | KeyDown Keys.Space -> model |> cam (CameraMessage.StartMove (V3d(0.0, 0.0, 2.0 * moveSpeed)))

        | Resize s -> 
            { model with 
                size = s
                proj = Frustum.perspective 90.0 0.1 1000.0 (float s.X / float s.Y) 
            }

        | UpdateTime(t, dt) ->
            let model = model |> cam (CameraMessage.UpdateTime(t, dt))
            if model.onFloor then
                { model with
                    camera = { model.camera with move = model.camera.move.XYO }
                    time = t
                }
            else
                let cam = model.camera
                { model with
                    time = t
                    camera = { cam with velocity = cam.velocity - V3d(0.0, 0.0, 25.81) * dt; move = cam.move.XYO }
                }

        | KeyDown _ 
        | KeyUp _ 
        | MouseDown _ 
        | MouseUp _ -> 
            model

    let view (env : Environment<Message>) (model : AdaptiveModel) =
        events env
        Sg.fullScreenQuad
        |> Sg.scale 100.0
        |> Sg.diffuseTexture DefaultTextures.checkerboard
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.diffuseTexture
        }
        //|> Sg.trafo (model.time |> AVal.map Trafo3d.RotationZ)
        |> Sg.viewTrafo (model.camera.camera |> AVal.map CameraView.viewTrafo)
        |> Sg.projTrafo (model.proj |> AVal.map Frustum.projTrafo)

