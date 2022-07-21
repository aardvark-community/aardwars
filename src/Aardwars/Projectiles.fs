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
                if pi.Owner = myName then 
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
                else Some {pi with Position=p1}
            else None
        )

    let explode (e : ExplosionInfo) (myName : string) (myPos : V3d) (otherPlayers : HashMap<string, OtherPlayerInfo>) =
        let smallSphere = Sphere3d(e.Position, e.SmallRadius)
        let bigSphere = Sphere3d(e.Position, e.BigRadius)
        let dirMag = V3d(1.0,1.0,1.0)
        let processPlayer (pos : V3d) (isMe : bool) =
            let bb = playerBounds.Translated pos
            let Smd = 
                if bb.Intersects smallSphere then
                    let dir = (pos - e.Position).Normalized
                    let vel = dirMag * dir * 10.0
                    let dmg = if isMe then e.BigDamage * 0.0 else e.SmallDamage
                    Some (vel, dmg)
                else None
            let Bmd =
                if bb.Intersects bigSphere then
                    let dir = (pos - e.Position).Normalized
                    let vel = dirMag * dir * 10.0
                    let dmg = if isMe then e.BigDamage * 0.0 else e.BigDamage
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
        let trafos = 
            projectiles |> ASet.toAVal |> AVal.map (Seq.toArray >> (Array.map (fun pi -> 
                Trafo3d.Translation pi.Position
            )))

        Sg.box' C4b.Yellow (Box3d.FromCenterAndSize(V3d.OOO,V3d.III*0.15))
        |> Sg.instanced trafos
        |> Sg.shader {
            do! DefaultSurfaces.trafo
            do! DefaultSurfaces.vertexColor
        }