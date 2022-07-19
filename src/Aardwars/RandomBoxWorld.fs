namespace Aardwars

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Rendering.Text
open Aardvark.SceneGraph.IO
open System
open System.Reflection
open System.Text
open System.Text.RegularExpressions


type Material = 
    | Bricks
    | Lava
    | Grass
    | Rocks
    | Water
    | Steelwall
    | Steelfloor

module Scene = 

    let boxSg (cubes : (Trafo3d * Material)[]) = 
        let grasstexture =
            PixTexture2d(PixImageMipMap [| PixImage.Load(@"assets\grasstexture.jpg") |], true) :> ITexture
            |> AVal.constant
        let lavatexture =
            PixTexture2d(PixImageMipMap [| PixImage.Load(@"assets\lavatexture.jpg") |], true) :> ITexture
            |> AVal.constant
        let rockstexture =
            PixTexture2d(PixImageMipMap [| PixImage.Load(@"assets\rockstexture.jpg") |], true) :> ITexture
            |> AVal.constant
        let brickstexture =
            PixTexture2d(PixImageMipMap [| PixImage.Load(@"assets\brickstexture.jpg") |], true) :> ITexture
            |> AVal.constant
        let watertexture =
            PixTexture2d(PixImageMipMap [| PixImage.Load(@"assets\watertexture.jpg") |], true) :> ITexture
            |> AVal.constant
        let steelwalltexture =
            PixTexture2d(PixImageMipMap [| PixImage.Load(@"assets\steelwalltexture.jpg") |], true) :> ITexture
            |> AVal.constant
        let steelfloortexture =
            PixTexture2d(PixImageMipMap [| PixImage.Load(@"assets\steelfloortexture.jpg") |], true) :> ITexture
            |> AVal.constant

        cubes
        |> Array.groupBy(fun (_,m) -> m)
        |> Array.map(fun (material,tupples) ->  
            let tex = 
                match material with
                | Bricks -> brickstexture
                | Lava   -> lavatexture
                | Grass  -> grasstexture
                | Rocks  -> rockstexture
                | Water  -> watertexture
                | Steelwall  -> steelwalltexture
                | Steelfloor  -> steelfloortexture
            let (trafos,_) = tupples|> Array.unzip
            Sg.box' C4b.Red Box3d.Unit
            |> Sg.diffuseTexture tex
            |> Sg.shader {
                do! DefaultSurfaces.stableTrafo
                do! DefaultSurfaces.diffuseTexture
                do! Shader.foggy
                //do! DefaultSurfaces.simpleLighting
            }
            |> Sg.instanced (AVal.constant trafos)
        )
        |> Sg.ofArray

    let createTestScene blockCount gridSize =
        let randomMaterial = System.Random()
        let materials = [|Material.Bricks;Material.Grass;Material.Lava;Material.Rocks;Material.Water;Material.Steelwall;Material.Steelfloor|]
        
        let grid (gridSize : V2i) (blockCount : int) =
            let rand = RandomSystem()
            let grid = Array2D.create gridSize.X gridSize.Y 1
            for i in 0 .. blockCount - 1 do
                let x = int <| clamp 0.0 (float gridSize.X - 1.0) (round <| rand.Gaussian(float gridSize.X/2.0, float gridSize.X/4.0))
                let y = int <| clamp 0.0 (float gridSize.Y - 1.0) (round <| rand.Gaussian(float gridSize.Y/2.0, float gridSize.Y/4.0))

                grid.[x,y] <- grid.[x,y] + 1
            grid
    
        let createCube x y z offset =
            let p = 
                //Trafo3d.Scale(1.01) *
                Trafo3d.Translation(float x, float y, float z) *
                Trafo3d.Translation(V3d(-offset, 0.0))
            p, (materials.[randomMaterial.Next(0,materials.Length)])
        let fullTrafos (grid : int[,]) (gridSize : V2i) =
            let offset = V2d gridSize / 2.0
            [|
                for x in 0 .. gridSize.X - 1 do
                    for y in 0 .. gridSize.Y - 1 do
                        let cnt = grid.[x,y]
                        for z in 0 .. cnt - 1 do
                            createCube x y z offset
            |]
        let sparseTrafos (grid : int[,]) (gridSize : V2i) =
            let offset = V2d gridSize / 2.0
            [|
                for x in 0 .. gridSize.X - 1 do
                    for y in 0 .. gridSize.Y - 1 do
                        let cnt = grid.[x,y]
                        for z in 0 .. cnt - 1 do
                            let left =
                                match x > 0 with
                                | true -> z < grid.[x-1,y]
                                | false -> false
                            let right =
                                match x < gridSize.X - 2 with
                                | true -> z < grid.[x+1,y]
                                | false -> false
                            let front =
                                match y < gridSize.Y - 2 with
                                | true -> z < grid.[x,y+1]
                                | false -> false
                            let behind =
                                match y > 0 with
                                | true -> z < grid.[x,y-1]
                                | false -> false
                            let top = z < cnt - 1
                           
                            if not (left && right && front && behind && top) then 
                                createCube x y z offset
            |]
            
        let fullGrid = grid gridSize blockCount
        Log.startTimed "Create lvl"
        let trafos = sparseTrafos fullGrid gridSize
        let worldBoxes = 
            trafos |> Array.map (fun (t,_) -> 
                let b = Box3d.Unit.Transformed(Trafo3d.Scale(1.01) * t)
                //Box3d.FromCenterAndSize(b.Center, b.Size + V3d.III * 0.01)
                b
            )
        Log.stop()

        worldBoxes, trafos
