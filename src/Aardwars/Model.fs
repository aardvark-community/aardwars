namespace Elm

open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Application
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Base
open Aardwars

type World =
    {
        Hit : V3d -> V3d -> V3d * bool
        Scene : IRenderWindow -> ISg
    }

module World =
    let plane(z : float) =
        let hit (p0 : V3d) (p1 : V3d) =
            let mutable p1 = p1
            if p1.Z <= z then
                p1.Z <- z
                p1, true
            else
                p1, false
        let sg = 
            Sg.fullScreenQuad
            |> Sg.scale 100.0
            |> Sg.diffuseTexture DefaultTextures.checkerboard
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }
        {
            Hit = hit
            Scene = fun _ -> sg
        }

    let randomGenerated blockCount gridSize (playerHeight : float) =
        let worldBoxes, render = Scene.createTestScene blockCount gridSize
        let sg = Scene.boxSg render
        let hit (p0 : V3d) (p1 : V3d) =
            let p0 = p0 - (V3d.OOI * playerHeight)
            let mutable p1 = p1 - (V3d.OOI * playerHeight)

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
            let newP1 = p1 + (V3d.OOI * playerHeight)
            let onFloor =                        
                if zUp then true
                else newP1.Z <= playerHeight
            newP1,onFloor
        {
            Hit = hit
            Scene = fun _ -> sg
        }
    
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
        [<NonAdaptive>]
        world       : World
        onFloor     : bool
        time        : float
        size        : V2i
        camera      : CameraModel
        proj        : Frustum
    }


