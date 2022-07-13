namespace Elm

open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Rendering
open Aardvark.Base


type World =
    abstract Hit : p0 : V3d * p1 : V3d -> V3d * bool


type PlaneWorld(z : float) =
    interface World with
        member x.Hit(p0 : V3d, p1 : V3d) =
            let mutable p1 = p1
            if p1.Z <= z then
                p1.Z <- z
                p1, true
            else
                p1, false

[<ModelType>]
type CameraModel =
    {
        look        : bool
        move        : V3d
        velocity    : V3d
        camera      : CameraView
    }

[<ModelType>]
type Model =
    {
        world       : World
        onFloor     : bool
        time        : float
        size        : V2i
        camera      : CameraModel
        proj        : Frustum
    }


