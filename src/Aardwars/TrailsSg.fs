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
    
    let sg (shotTrails : aset<TrailInfo>) (currentTime : aval<float>)=
        
        let verts =
            shotTrails |> ASet.toAVal |> AVal.map (Seq.toArray >> Array.collect (fun ti -> 
                [|ti.offsetLine.P0; ti.offsetLine.P1|]
            ))
        
        let colors = 
            shotTrails 
            |> ASet.toAVal 
            |> AVal.map (fun hs -> 
                hs 
                |> HashSet.toArray
                |> Array.collect(fun st -> [|st.color;st.color|])
            )

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

module HitEnemyMarkerInstance =
    let scene (t : aval<float>) (insts : amap<string,float>) =
        let onoff = 
            let m = insts |> AMap.toASetValues |> ASet.tryMax 
            m |> AVal.bind (fun m -> 
                match m with 
                | None -> AVal.constant false
                | Some m -> t |> AVal.map (fun t -> t-m < PlayerConstant.hitEnemyMarkerDuration)
            )
        let pos = 
            let v0 = 0.055
            let v1 = 0.085
            [|
                V3d( v0, v0,0.0);V3d( v1, v1,0.0)
                V3d( v0,-v0,0.0);V3d( v1,-v1,0.0)
                V3d(-v0,-v0,0.0);V3d(-v1,-v1,0.0)
                V3d(-v0, v0,0.0);V3d(-v1, v1,0.0)
            |]
        Sg.draw IndexedGeometryMode.LineList
        |> Sg.vertexAttribute' DefaultSemantic.Positions pos
        |> Sg.shader {
            do! DefaultSurfaces.constantColor C4f.White
            do! DefaultSurfaces.thickLine
        }
        |> Sg.uniform' "LineWidth" 4.0
        |> Sg.onOff onoff
        |> Sg.viewTrafo' Trafo3d.Identity
        |> Sg.projTrafo' Trafo3d.Identity
        |> Sg.pass Passes.pass3


module GotHitIndicatorInstance =
    let scene (t : aval<float>) (fw : aval<V3d>) (loc : aval<V3d>) (insts : aset<GotHitIndicatorInstance>) =
        let tri = 
            let pos = [|V3d(0.0,0.5,0.0);V3d(0.05,0.45,0.0);V3d(-0.05,0.45,0.0)|]
            let col = [|C4f(1.0f,0.0f,0.0f,1.0f);C4f(0.8f,0.0f,0.0f,0.6f);C4f(0.8f,0.0f,0.0f,0.6f)|]
            Sg.draw IndexedGeometryMode.TriangleList
            |> Sg.vertexAttribute' DefaultSemantic.Positions pos
            |> Sg.vertexAttribute' DefaultSemantic.Colors col

        let atts = 
            insts |> ASet.toAVal |> AVal.map (Seq.toArray >> (Array.map (fun inst -> 
                let damt = (clamp 0.0 1.0 (inst.damage/100.0))
                let trafo =
                    (fw,loc) ||> AVal.map2 (fun fw loc -> 
                        let a = fw.XYO
                        let b = (inst.HitPosition - loc).Normalized.XYO
                        let ang = (atan2 (a.X*b.Y - a.Y*b.X) (a.X*b.X + a.Y*b.Y))
                        let ds = 0.75 + damt * 0.25
                        Trafo3d.Scale(ds) *
                        Trafo3d.RotationZ(ang)
                    )
                let alpha = 
                    t |> AVal.map (fun t -> 
                        let ta = 1.0-(clamp 0.0 1.0 (t - inst.StartTime) / PlayerConstant.gotHitMarkerDuration)
                        let da = 0.5 + damt * 0.5
                        float32(ta*da)
                    )
                trafo,alpha
            )))

        let trafos = 
            AVal.custom (fun tok -> atts.GetValue(tok) |> Array.map (fun (t,_) -> t.GetValue(tok)))
        let alphas = 
            AVal.custom (fun tok -> atts.GetValue(tok) |> Array.map (fun (_,a) -> a.GetValue(tok)))
        let things = 
            Map.ofList [
                "ModelTrafo", (typeof<Trafo3d>,trafos |> AVal.map unbox)
                "Alpha", (typeof<float32>,alphas |> AVal.map unbox)
            ]

        let triSg =
            tri
            |> Sg.instanced' things
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                do! Elm.Shader.sgAlpha
            }
            |> Sg.blendMode' BlendMode.Blend

        let borderSg =
            let poss v = 
                [|
                    V3d( v,-v,0.0)
                    V3d( v, v,0.0)
                    V3d(-v, v,0.0)
                    V3d(-v,-v,0.0)
                    V3d( v,-v,0.0)
                |]
            let border1 = 
                let onoff = 
                    let m = insts |> ASet.map (fun i -> i.StartTime) |> ASet.tryMax |> AVal.map (Option.defaultValue 0.0)
                    (m,t) ||> AVal.map2 (fun m t -> let rem = t-m in rem < 0.037)
                    
                let pos = poss 0.995
                let col = Array.replicate pos.Length C4b.Crimson
                Sg.draw IndexedGeometryMode.LineStrip
                |> Sg.vertexAttribute' DefaultSemantic.Positions pos
                |> Sg.vertexAttribute' DefaultSemantic.Colors col
                |> Sg.onOff onoff
            let border2 = 
                let onoff = 
                    let m = insts |> ASet.map (fun i -> i.StartTime) |> ASet.tryMax |> AVal.map (Option.defaultValue 0.0)
                    (m,t) ||> AVal.map2 (fun m t -> let rem = t-m in rem < 0.075 && rem >= 0.037)
                    
                let pos = poss 0.97
                let col = Array.replicate pos.Length C4b.Crimson
                Sg.draw IndexedGeometryMode.LineStrip
                |> Sg.vertexAttribute' DefaultSemantic.Positions pos
                |> Sg.vertexAttribute' DefaultSemantic.Colors col
                |> Sg.onOff onoff
            Sg.ofList [border1;border2] 
            |> Sg.shader {
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.thickLine
            }
            |> Sg.uniform' "LineWidth" 15.0


        Sg.ofList [triSg;borderSg]
        |> Sg.viewTrafo' Trafo3d.Identity
        |> Sg.projTrafo' Trafo3d.Identity
        |> Sg.pass Passes.pass3