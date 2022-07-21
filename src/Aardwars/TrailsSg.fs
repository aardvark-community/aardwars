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


module Trails =
    let random = System.Random ()
    module Shader =
        open FShade

        type TrailVertex = 
            {
                [<Color>] c : V4d
                [<Semantic("Alphas")>] t : float32
            }

        let adjustAlpha (v : TrailVertex) =
            vertex {
                return {v with c = V4d(v.c.X, v.c.Y, v.c.Z, float v.t)}
            }

    let sg (shotTrails : aset<TrailInfo>) (currentTime : aval<float>)=
        let verts =
            shotTrails |> ASet.toAVal |> AVal.map (Seq.toArray >> Array.collect (fun ti -> 
                [|ti.line.P0; ti.line.P1|]
            ))
        
        let colors = 
            shotTrails 
            |> ASet.toAVal 
            |> AVal.map (fun hs -> 
                hs 
                |> HashSet.map(fun st -> 
                    let c = st.color
                    printfn "trail color: %A" c
                    c
                )
                |> HashSet.toArray
            )
       
        //let colors2 =
        //    verts |> AVal.map (fun vs -> Array.replicate vs.Length C4b.Yellow)

        let ts =
            (shotTrails |> ASet.toAVal,currentTime) ||> AVal.map2 (fun trails time -> 
                trails 
                |> Seq.toArray 
                |> Array.collect (fun ti -> 
                    let maxTime = ti.startTime + ti.duration
                    let elapsed = maxTime - time
                    if elapsed > 0.0 then 
                        let t = float32 <| elapsed / ti.duration
                        [|t;t**2.0f|]
                    else
                        [|0.0f;0.0f|]
                )
            )
        
        Sg.draw IndexedGeometryMode.LineList
        |> Sg.vertexAttribute DefaultSemantic.Positions verts
        |> Sg.vertexAttribute DefaultSemantic.Colors colors
        |> Sg.vertexAttribute "Alphas" ts
        |> Sg.shader{
            do! DefaultSurfaces.trafo
            do! Shader.adjustAlpha
            do! DefaultSurfaces.vertexColor
            do! DefaultSurfaces.thickLine
        }
        |> Sg.blendMode' BlendMode.Blend
        |> Sg.uniform' "LineWidth" (3.0)
        |> Sg.pass Passes.pass1