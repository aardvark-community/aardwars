namespace Aardvars

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


module EmbeddedResource =

    let get (path : string) =
        let asm = Assembly.GetExecutingAssembly()
        let name = Regex.Replace(asm.ManifestModule.Name, @"\.(exe|dll)$", "", RegexOptions.IgnoreCase)
        let path = Regex.Replace(path, @"(\\|\/)", ".")
        let stream = asm.GetManifestResourceStream(name + "." + path)
        if stream <> null then stream
        else failwithf "Cannot open resource stream with name '%s'" path

    let loadPixImage<'T> (path : string) =
        use stream = get path
        PixImage.Load(stream).AsPixImage<'T>()

    let getTexture (textureParams : TextureParams) (path : string) =
        let openStream = fun () -> get path
        StreamTexture(openStream, textureParams) :> ITexture

module Import =
    
    open Loader
    type Node with
        member x.SubstituteMaterial (f : Material -> Option<Material>) =
            match x with
                | Trafo(t,n) ->
                    Trafo(t, n.SubstituteMaterial f)
                | Material(m,n) ->
                    let n = n.SubstituteMaterial f
                    match f m with
                        | Some m -> Material(m, n)
                        | None -> Material(m,n)
                | Leaf m ->
                    Leaf m
                | Group nodes ->
                    nodes |> List.map (fun n -> n.SubstituteMaterial f) |> Group
                | Empty ->
                    Empty
    type Scene with
        member x.SubstituteMaterial (f : Material -> Option<Material>) =
            { x with root = x.root.SubstituteMaterial f }

        member x.AddTextures (repl : list<Symbol * Texture>) =
            let map = Map.ofList repl
            x.SubstituteMaterial (fun m -> Some { m with textures = Map.union  m.textures map })

    let importObj (p : string) =
        let flags = 
            Assimp.PostProcessSteps.Triangulate |||
            Assimp.PostProcessSteps.FindInvalidData

        let loaded = Loader.Assimp.Load(p, flags)

        let tex = { Loader.coordIndex = 0; Loader.texture = FileTexture(@"assets\gun.png", TextureParams.mipmapped) :> ITexture }
        let loaded = 
            loaded.AddTextures [
                DefaultSemantic.DiffuseColorTexture, tex
            ]

        Sg.ofList [
            Sg.adapter loaded
        ]
    //{ Loader.coordIndex = 0; Loader.texture = FileTexture(Path.Combine(basePath, n), TextureParams.mipmapped) :> ITexture }

    let importGun() =  importObj @"assets\gun.obj"

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
