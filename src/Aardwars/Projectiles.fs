namespace Aardwars

open Adaptify
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open FSharp.Data.Adaptive
open Aardvark.Application
open Elm
open System.Reflection
open Aardvark.Rendering.Text

module Projectile =
    let inline hitBox (ray : FastRay3d) (box : Box3d) (maxT : float) =
        let mutable t0 = 0.0
        let mutable t1 = 0.0
        if ray.Intersects(box,&t0,&t1) then 
            if t0 >= 0.0 && t0 <= maxT then 
                (true, t0)
            elif t1 >= 0.0 && t1 <= maxT then 
                (true, t1)
            else 
                (false, 0.0)
        else 
            (false, 0.0)
    
    let calculateHits 
        (projectiles : HashSet<ProjectileInfo>) 
        (myName : string)
        (time : float) 
        (dt : float) 
        (world : World) 
        (otherPlayers : HashMap<string,OtherPlayerInfo>) 
        (emitExplosion : ExplosionInfo -> unit) =
        
        projectiles |> HashSet.choose (fun pi -> 
            if pi.StartTime+pi.MaxDuration > time then 
                let p0 = pi.Position
                let p1 = p0+dt*pi.Velocity
                let ray = Ray3d(p0, (p1-p0).Normalized)
                let fr = FastRay3d(ray)
                let p1t = ray.GetTOfProjectedPoint p1
                let closestPlayerT =
                    otherPlayers |> Seq.map (fun (_,pi) -> playerBounds.Translated pi.pos) |> Seq.choose (fun bound -> 
                        match hitBox fr bound p1t with 
                        | (true,t) -> Some t
                        | (false,_) -> None
                    )
                    |> Seq.sort
                    |> Seq.tryHead
                let closestWorldT =
                    world.Intersections ray 0.0 p1t 
                    |> Seq.sortBy fst 
                    |> Seq.tryHead
                    |> Option.map (fun (t,_) -> t)
                let hitT = [closestPlayerT; closestWorldT] |> List.choose id |> List.sort |> List.tryHead
                match hitT with 
                | None -> Some {pi with Position=p1}
                | Some hit -> 
                    if pi.Owner = myName then 
                        let hitPos = ray.GetPointOnRay hit
                        let explosion = 
                            {
                                Owner = pi.Owner
                                Position = hitPos
                                SmallRadius = pi.ExplosionSmallRadius
                                BigRadius = pi.ExplosionBigRadius
                                SmallDamage = pi.ExplosionSmallDamage
                                BigDamage = pi.ExplosionBigDamage
                            }
                        emitExplosion explosion
                    None
            else None
        )

    let explode (e : ExplosionInfo) (myName : string) (myPos : V3d) (otherPlayers : HashMap<string, OtherPlayerInfo>) =
        let smallSphere = Sphere3d(e.Position, e.SmallRadius)
        let bigSphere = Sphere3d(e.Position, e.BigRadius)
        let processPlayer (pos : V3d) (isMe : bool) =
            let bb = playerBounds.Translated pos
            let Smd = 
                if bb.Intersects smallSphere then
                    let dir = (pos - e.Position).Normalized
                    let vel = dir * 0.15
                    let dmg = if isMe then e.BigDamage * 0.35 else e.SmallDamage
                    Some (vel, dmg)
                else None
            let Bmd =
                if bb.Intersects bigSphere then
                    let dir = (pos - e.Position).Normalized
                    let vel = dir * 0.15
                    let dmg = if isMe then e.BigDamage * 0.35 else e.BigDamage
                    Some (vel, dmg)
                else None
            match Smd, Bmd with 
            | Some (svel,sdmg), Some (bvel,bdmg) -> Some (svel+bvel,sdmg+bdmg)
            | None, Some (bvel,bdmg) -> Some (bvel,bdmg)
            | _ -> None
        let myHit = processPlayer myPos (e.Owner=myName)
        let otherHits = 
            otherPlayers |> HashMap.choose (fun name info -> processPlayer info.pos (e.Owner=name))
        myHit,otherHits
                
        
    let scene (projectiles : aset<ProjectileInfo>) =
        let pa = projectiles |> ASet.toAVal |> AVal.map Seq.toArray
        let projsSg =
            let trafos = 
                pa |> AVal.map (Array.map (fun pi -> 
                    Trafo3d.Translation pi.Position
                ))

            Sg.box' C4b.Yellow (Box3d.FromCenterAndSize(V3d.OOO,V3d.III*0.15))
            |> Sg.instanced trafos
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
            }
        let trailsSg =
            let atts = 
                pa |> AVal.map (Array.collect (fun pi -> 
                    let pit = pi.Trail |> List.toArray
                    let l = float pit.Length
                    if pit.Length <= 0 then [||]
                    else
                        [|
                            let t = 1.0/1.5
                            yield pi.Position,C4b.White,t
                            let mutable lastP = pit.[0]
                            yield lastP,C4b.White,t
                            for i in 1..pit.Length-1 do
                                if i=1 then 
                                    let t = (1.0-(float i/l))
                                    yield lastP,C4b.White,t
                                    yield pit.[i],C4b.White,t/1.5
                                else
                                    let t = (1.0-(float (i-1)/l))
                                    yield pit.[i-1],C4b.Yellow,t/1.5
                                    let t = (1.0-(float i/l))
                                    yield pit.[i],C4b.GoldenRod,t/1.5
                        |]
                ))
            let atts = atts |> AVal.map Array.unzip3
            let pos  = atts |> AVal.map (fun (v,_,_) -> v)
            let cols = atts |> AVal.map (fun (_,v,_) -> v)
            let alps = atts |> AVal.map (fun (_,_,v) -> v)

            Sg.draw IndexedGeometryMode.LineList
            |> Sg.vertexAttribute DefaultSemantic.Positions pos
            |> Sg.vertexAttribute DefaultSemantic.Colors cols
            |> Sg.vertexAttribute "Alphas" alps
            |> Sg.shader{
                do! DefaultSurfaces.trafo
                do! Elm.Shader.adjustAlpha
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.thickLine
            }
            |> Sg.uniform' "LineWidth" (8.0)
            |> Sg.blendMode' BlendMode.Blend
            |> Sg.pass Passes.pass1
            
        Sg.ofList [projsSg;trailsSg]

    let explosionScene (t : aval<float>) (es : aset<ExplosionAnimationInfo>) =
        let arr = 
            es |> ASet.map (fun e -> 
                let inline res (r : float) (c : C4b) (t : float) (rs : float) =
                    let color = c
                    let t = (1.0 - (t - e.StartTime)/e.Duration)
                    let alpha = t * 0.95
                    let localScale = t * 0.75 + 0.25
                    let trafo = 
                        Trafo3d.RotationZInDegrees(t*2.0*360.0*rs) *
                        Trafo3d.Scale r *
                        Trafo3d.Scale localScale *
                        Trafo3d.Translation e.Center
                    trafo,color,alpha
                t |> AVal.map (fun t -> 
                    [|
                        res e.SmallRadius e.SmallColor t -1.0
                        res e.BigRadius e.BigColor t 1.0
                    |]
                )
            )
        let things = arr |> ASet.toAVal 
        let trafos = 
            AVal.custom (fun tok -> 
                let t = things.GetValue(tok) |> HashSet.toArray
                t |> Array.collect (fun atts -> 
                    atts.GetValue(tok) |> Array.map (fun (v,_,_) -> v)
                )   
            )
        let colors = 
            AVal.custom (fun tok -> 
                let t = things.GetValue(tok) |> HashSet.toArray
                t |> Array.collect (fun atts -> 
                    atts.GetValue(tok) |> Array.map (fun (_,v,_) -> v)
                )   
            )
        let alphas = 
            AVal.custom (fun tok -> 
                let t = things.GetValue(tok) |> HashSet.toArray
                t |> Array.collect (fun atts -> 
                    atts.GetValue(tok) |> Array.map (fun (_,_,v) -> float32 v)
                )   
            )
        let things = 
            Map.ofList [
                "ModelTrafo", (typeof<Trafo3d>,trafos |> AVal.map unbox)
                "Color", (typeof<C4b>,colors |> AVal.map unbox)
                "Alpha", (typeof<float32>,alphas |> AVal.map unbox)
            ]
        Sg.sphere' 1 C4b.White 1.0
        |> Sg.instanced' things
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.sgColor
            do! Elm.Shader.sgAlpha
        }
        |> Sg.blendMode' BlendMode.Blend
        |> Sg.pass Passes.pass1