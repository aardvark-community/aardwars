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


[<AutoOpen>]
module PlayerConstant =
    let playerBounds = Box3d(V3d(-0.3, -0.3, -1.7), V3d(0.3, 0.3, 0.0))
    let killfeedLength = 10
    let killfeedFadeTime = 16.0
    let gotHitMarkerDuration = 1.0
    let hitEnemyMarkerDuration = 0.125
    let respawnDelay = 3.0
    let roundTime = 390.0
    let roundRestartDelay = 5.0
    
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

    let loadTexture (texture : string) =
        printfn "UPLOADED TEXTURE"
        //{ Loader.coordIndex = 0; Loader.texture = FileTexture((sprintf @"assets\%s.png" texture), TextureParams.mipmapped) :> ITexture }
        PixTexture2d(PixImageMipMap [|PixImage.Load texture|], true) :> ITexture

    let importObj (name : string) =
        printfn "IMPORTED"
        let flags = 
            Assimp.PostProcessSteps.Triangulate |||
            Assimp.PostProcessSteps.FindInvalidData

        let loaded = 
            Loader.Assimp.Load((sprintf @"assets\%s.obj" name), flags)

        //let tex = { Loader.coordIndex = 0; Loader.texture = FileTexture((sprintf @"assets\%s.png" texture), TextureParams.mipmapped) :> ITexture }
        //let loaded = 
        //    loaded.AddTextures [
        //        DefaultSemantic.DiffuseColorTexture, tex
        //    ]

        loaded
        
    let importGun (name : string) = importObj name
    let importPlayer = importObj "player"