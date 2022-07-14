namespace Aardwars

open Aardvark.Base
open Aardvark.Base.Sorting
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.SceneGraph
open Microsoft.FSharp.NativeInterop
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

module MinecraftWorld =
    
    //@"C:\Users\Schorsch\Downloads\Notre Dame and Medieval City"
    //@"C:\Users\Schorsch\Desktop\mc"
    let load (rt : IRuntime) (regionsDir : string) (assetDir : string) =
        let blockTable = Block.loadMapping assetDir

        let sections =
            regionsDir
            |> Minecraft.getRegions
            //|> Seq.take 1
            |> Seq.collect Minecraft.enumerateRawNbtBuffers
            |> Seq.map Minecraft.parseRawNbtBuffer
            |> Seq.collect Minecraft.extractSectionsFromChunk
            |> Seq.toArray

        let mutable tree = Octree.empty 1024 (fun (bi : BoxInfo) -> bi.BoundingBox)

        let states = System.Collections.Generic.HashSet()
        let mutable iter = 0

        let textureList = System.Collections.Generic.List()
        let textureDict = Dict<string, int>()

        let getTextureId(name : string) =
            textureDict.GetOrCreate(name, fun name ->
                let id = textureList.Count
                textureList.Add name
                id
            )

        sections 
        |> Array.iter (fun s ->
            let off = V3s(s.XPos, s.ZPos, s.Y) * 16s
                
            let exists x y z =
                if x < 0 || y < 0 || z < 0 || x > 15 || y > 15 || z > 15 then 
                    false
                else
                    let idx = y*256 + z*16 + x
                    not (s.BlockStates.[idx].Contains "air")
                    //s.BlockStates.[idx] <> "minecraft:air"

            let chunk =
                [|
                    let mutable i = 0
                    for y in 0 .. 15 do
                        for z in 0 .. 15 do
                            for x in 0 .. 15 do
                                states.Add s.BlockStates.[i] |> ignore
                                if not (s.BlockStates.[i].Contains "air") then
                                    let occluded =
                                        exists (x - 1) y z &&
                                        exists (x + 1) y z &&
                                        exists x (y + 1) z &&
                                        exists x (y - 1) z &&
                                        exists x y (z + 1) &&
                                        exists x y (z - 1) &&
                                        
                                        exists (x - 1) (y + 1) z &&
                                        exists (x + 1) (y + 1) z &&
                                        exists (x - 1) (y - 1) z &&
                                        exists (x + 1) (y - 1) z &&
                                        
                                        exists (x - 1) y (z + 1) &&
                                        exists (x + 1) y (z + 1) &&
                                        exists (x - 1) y (z - 1) &&
                                        exists (x + 1) y (z - 1) &&
                                        
                                        exists x (y - 1) (z + 1) &&
                                        exists x (y + 1) (z + 1) &&
                                        exists x (y - 1) (z - 1) &&
                                        exists x (y + 1) (z - 1)

                                    if not occluded then
                                        let mat = s.BlockStates.[i]
                                        let info = 
                                            match blockTable.TryGetValue mat with
                                            | (true, t) -> t
                                            | _ -> Unknown

                                        let texIds = 
                                            match info with
                                            | Unknown -> V3s(-1s, -1s, -1s)
                                            | All t | Cross t ->
                                                let id = getTextureId t
                                                V3s(int16 id, int16 id, int16 id)
                                            | BottomTop(bottom, top, side) ->
                                                let bid = getTextureId bottom
                                                let tid = getTextureId top
                                                let sid = getTextureId side
                                                V3s(int16 bid, int16 tid, int16 sid)
                                            

                                        yield { Offset = off + V3s(x,z,y); MaterialId = texIds }
                                i <- i + 1
                |]
            tree <- Octree.add chunk tree
            iter <- iter + 1
            if iter % 100 = 0 then
                printfn "chunk at %A: %d" off tree.Count
        )


        let atlas =
            use __ = rt.ContextLock
            let atlas = rt.CreateTexture2DArray(V2i(16, 16), TextureFormat.Rgba8, 4, 1, textureList.Count)
            for i in 0 .. textureList.Count - 1 do  
                try
                    let img = PixImageSharp.Create(textureList.[i]).ToPixImage<byte>(Col.Format.RGBA).SubImage(V2i.Zero, V2i(16,16))

                    if textureList.[i].ToLower().Contains "_leaves" then 
                        img.GetMatrix<C4b>().SetMap(img.GetMatrix<C4b>(), fun c -> C4b(0uy, c.G, 0uy, c.A)) |> ignore


                        
                    rt.Upload(atlas, img, level = 0, slice = i)
                    //let mutable s = img.Size
                    //let mutable img = img
                    //for l in 0 .. 3 do
                    //    env.Runtime.Upload(atlas, img, level = l, slice = i)
                    //    let s = img.Size/2
                    //    if s.AllGreater 0 then
                    //        let nimg = PixImage<byte>(Col.Format.RGBA, img.Size/2)
                    //        NativeVolume.using img.Volume (fun pSrc ->
                    //            NativeVolume.using nimg.Volume (fun pDst ->
                    //                NativeVolume.blit pSrc pDst
                    //            )
                    //        )
                    //        img <- nimg
                with _ ->
                    Log.error "nasdhbaiksdnjasndkjasndkjasd: %A" textureList.[i]
            rt.GenerateMipMaps atlas
            atlas
        tree,atlas   
    