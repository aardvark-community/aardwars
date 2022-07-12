namespace Aardvars

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Rendering.Text

module Scene = 

    let boxSg trafos = 

        Sg.box' C4b.Red Box3d.Unit
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.simpleLighting
        }
        |> Sg.instanced (AVal.constant trafos)


    let createTestScene = 
        
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

        worldBoxes, trafos
