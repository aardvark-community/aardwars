﻿namespace Aardwars

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
                [|ti.line.P0; ti.line.P1|]
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
                        let ta = 1.0-(clamp 0.0 1.0 (t - inst.StartTime) / PlayerConstant.hitMarkerDuration)
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
            let onoff = 
                let m = insts |> ASet.map (fun i -> i.StartTime) |> ASet.tryMax |> AVal.map (Option.defaultValue 0.0)
                (m,t) ||> AVal.map2 (fun m t -> t-m < 0.075)
                    
            let v = 0.98
            let pos = 
                [|
                    V3d( v,-v,0.0)
                    V3d( v, v,0.0)
                    V3d(-v, v,0.0)
                    V3d(-v,-v,0.0)
                    V3d( v,-v,0.0)
                |]
            let col = Array.replicate pos.Length C4b.Crimson
            Sg.draw IndexedGeometryMode.LineStrip
            |> Sg.vertexAttribute' DefaultSemantic.Positions pos
            |> Sg.vertexAttribute' DefaultSemantic.Colors col
            |> Sg.shader {
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.thickLine
            }
            |> Sg.uniform' "LineWidth" 6.0
            |> Sg.onOff onoff


        Sg.ofList [triSg;borderSg]
        |> Sg.viewTrafo' Trafo3d.Identity
        |> Sg.projTrafo' Trafo3d.Identity
        |> Sg.pass Passes.pass3