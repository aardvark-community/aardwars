namespace Aardwars


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
     