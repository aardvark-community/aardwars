namespace Aardwars

open Elm
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

    let names =
        let assembly = typeof<NetworkClient>.Assembly
        let prefix = sprintf "%s." (assembly.GetName().Name)
        assembly.GetManifestResourceNames() |> Array.choose (fun n ->
            if n.StartsWith prefix then
                let name = n.Substring prefix.Length

                let parts = name.Split('.', System.StringSplitOptions.RemoveEmptyEntries)
                let name =
                    if parts.Length >= 2 then sprintf "%s.%s" parts.[parts.Length - 2] parts.[parts.Length - 1]
                    else name
                Log.warn "%A" name
                Some(name, fun () -> assembly.GetManifestResourceStream(n))
            else
                None
        )
    open System.IO
    let loadTexture (texture : string) =
        let texName =
            Path.GetFileName(texture.Replace('/', Path.DirectorySeparatorChar).Replace('\\', Path.DirectorySeparatorChar))
            
        let op = 
            names |> Array.tryPick (fun (n, op) ->
                if n = texName then Some op
                else None
            )
            
        match op with
        | Some op ->
            use s = op()
            PixTexture2d(PixImageMipMap [|PixImage.Load s|], true) :> ITexture
        | None ->
            Log.warn "could not find texture: %A" texName
            DefaultTextures.checkerboard.GetValue()

    let importObj (name : string) =
        let name = Path.ChangeExtension(name, ".obj")
        let op = 
            names |> Array.tryPick (fun (n, op) ->
                
                if n = name then Some op
                else None
            )
            
        match op with
        | Some op ->
            let dst = Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "aardwars")
            if not (Directory.Exists dst) then Directory.CreateDirectory dst |> ignore
            
            let tmp = Path.Combine(dst, name)
            do
                use s = op()
                use w = File.OpenWrite tmp
                s.CopyTo w
                
                
            printfn "IMPORTED"
            let flags = 
                Assimp.PostProcessSteps.Triangulate |||
                Assimp.PostProcessSteps.FindInvalidData

            let loaded = 
                Loader.Assimp.Load(tmp, flags)

            loaded
        | None ->
            failwithf "could not load asset %A" name
            
    let importGun (name : string) = importObj name
    let importPlayer = importObj "player"