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
open FShade

type AmmoInfo = 
    {
        reloadTime : float
        maxShots : int
        availableShots : int
    }

type AmmunitionType = 
    | Endless
    | Limited of AmmoInfo

type WeaponType =
    | Primary
    | Secondary

type TrailInfo = 
    {
        Line        :  Line3d
        startTime   :  float
        duration    :  float
    }

type Target =
    {
        currentHp : int
        maxHp : int
        pos : V3d
        radius : float
    }

type LastHitInfo = 
    {
        name        : string
        hitSeries   : int
    }

type Weapon =
    {
        damage           : Range1d
        name             : string
        cooldown         : float
        ammo             : AmmunitionType
        range            : float
        spray            : float
        canShoot         : AmmunitionType -> bool
        createHitrays    : CameraView -> list<Ray3d>
        createShottrails : list<Ray3d> -> CameraView -> float -> list<TrailInfo>
        findHitTargets   : list<Ray3d> -> HashMap<string, Target> ->  CameraView -> list<string>
        processHits      : list<string> -> HashMap<string, Target> -> HashMap<string, Target>
        updateAmmo       : AmmunitionType -> AmmunitionType
    }

module Weapon =
    let rand = RandomSystem()
    let laserGun =
        let range = 1000.0
        let damage = Range1d(10,20)
        let canShoot (t : AmmunitionType) : bool = 
            match t with
                | Endless -> true
                | Limited ammoInfo -> ammoInfo.availableShots > 0
        let createHitrays (cv : CameraView) : list<Ray3d> = 
            let p = cv.Location
            let d = cv.Forward
            [Ray3d(p, d)]
        let createShottrails (rays : list<Ray3d>) (cv : CameraView) time =
            rays |> List.map(fun shotRay ->
                let p0 = shotRay.Origin + cv.Right * 0.7 + cv.Down * 0.4
                
                let p1 = shotRay.Origin + range * shotRay.Direction
                let line = Line3d(p0, p1)
                {
                    Line = line
                    startTime = time
                    duration = 1.0
                }
            )
        let findHitTargets (rays : list<Ray3d>)(targets : HashMap<string, Target>) (cv : CameraView) : list<string> =
            rays |> List.choose(fun shotRay -> 
                targets 
                |> HashMap.choose (fun name t -> 
                    let s = Sphere3d(t.pos, t.radius)
                    //let r = t.radius
                    //let d = model.camera.camera.Forward
                    //let x = (model.camera.camera.Location - t.pos)
                    //let b = 2.0*d*x
                    //let c = -r*r+x*x
                    //let rs = (-b + sqrt (b*b-4.0*c))/2.0
                    let d0 = t.pos -  cv.Location
                    let d1 = cv.Forward
                    let inFront = (Vec.dot d0.Normalized d1.Normalized) > 0.0
                    let mutable tout = 0.0
                    let isHit = shotRay.HitsSphere(t.pos,t.radius,&tout)
                    match isHit && inFront with
                    |true -> Some tout
                    |false -> None
                ) 
                |> HashMap.toList
                |> List.sortBy snd
                |> List.tryHead
                |> Option.map fst
            )

        let processHits (names : list<string>) (targets : HashMap<string, Target>) : HashMap<string, Target> =
            let processed = 
                names 
                |> List.groupBy id
                |> List.map (fun (s,ss) -> 
                    let hitCount = ss |> List.length
                    targets
                    |> HashMap.alter s (fun altV -> 
                        match altV with
                        | None -> None
                        | Some target -> 
                            let totalDamage =
                                List.init hitCount (fun _ -> 
                                    let t = rand.UniformDouble()
                                    damage.Lerp t
                                )
                                |> List.sum
                            let newHp = float target.currentHp - totalDamage
                            Some {target with currentHp = int newHp}
                    )
                )
            (processed |> HashMap.unionMany)
        let updateAmmo (currentAmmo : AmmunitionType) =
            match currentAmmo with
            | Endless -> Endless
            | Limited ammoInfo -> 
                let updatedAmmoInfo =
                    match ammoInfo.availableShots > 0 with
                    | true -> ammoInfo.availableShots - 1
                    | false -> 0
                       
                Limited {ammoInfo with availableShots = updatedAmmoInfo}


        {
            damage      = damage
            name        = "Lasergun"
            cooldown    = 0.5
            ammo        = AmmunitionType.Endless
            range       = range
            spray       = 0.0
            canShoot    = canShoot
            createHitrays = createHitrays
            createShottrails = createShottrails
            findHitTargets = findHitTargets
            processHits = processHits
            updateAmmo = updateAmmo
        }

    let shotGun : Weapon =
        let range = 30.0
        let damage = Range1d(5, 12)
        let canShoot (t : AmmunitionType) : bool = 
            match t with
                | Endless -> true
                | Limited ammoInfo -> ammoInfo.availableShots > 0
        let createHitrays (cv : CameraView) : list<Ray3d> = 
            List.init 10 (fun _ ->
                let u = (rand.UniformDouble() * 2.0 - 1.0) * 0.1
                let v = (rand.UniformDouble() * 2.0 - 1.0) * 0.1
                let p = cv.Location
                let p0 = cv.Location + cv.Forward
                let p' = p0 + u * cv.Right + v * cv.Up
                let d' = p' - p
                Ray3d(p, d'.Normalized)
            )
        let createShottrails (rays : list<Ray3d>) (cv : CameraView) time =
            rays |> List.map(fun shotRay ->
                let p0 = shotRay.Origin + cv.Right * 0.3 + cv.Down * 0.15 + cv.Forward * 0.8
                
                let p1 = shotRay.Origin + range * shotRay.Direction
                let line = Line3d(p0, p1)
                {
                    Line = line
                    startTime = time
                    duration = 0.5
                }
            )
        let findHitTargets (rays : list<Ray3d>)(targets : HashMap<string, Target>) (cv : CameraView) : list<string> =
            rays |> List.choose(fun shotRay -> 
                targets 
                |> HashMap.choose (fun name t -> 
                    let s = Sphere3d(t.pos, t.radius)
                    //let r = t.radius
                    //let d = model.camera.camera.Forward
                    //let x = (model.camera.camera.Location - t.pos)
                    //let b = 2.0*d*x
                    //let c = -r*r+x*x
                    //let rs = (-b + sqrt (b*b-4.0*c))/2.0
                    let d0 = t.pos -  cv.Location
                    let d1 = cv.Forward
                    let inFront = (Vec.dot d0.Normalized d1.Normalized) > 0.0
                    let mutable tout = 0.0
                    let isHit = shotRay.HitsSphere(t.pos,t.radius,&tout)
                    let isHit = isHit && tout <= range
                    match isHit && inFront with
                    |true -> Some tout
                    |false -> None
                ) 
                |> HashMap.toList
                |> List.sortBy snd
                |> List.tryHead
                |> Option.map fst
            )

        let processHits (names : list<string>) (targets : HashMap<string, Target>) : HashMap<string, Target> =
            let processed = 
                names 
                |> List.groupBy id
                |> List.map (fun (s,ss) -> 
                    let hitCount = ss |> List.length
                    let target = targets.Item s
                    let totalDamage =
                        List.init hitCount (fun _ -> 
                            let t = rand.UniformDouble()
                            damage.Lerp t
                        )
                        |> List.sum
                    let newHp = float target.currentHp - totalDamage
                    s, {target with currentHp = int newHp}
                    

                    //targets
                    //|> HashMap.alter s (fun altV -> 
                    //    match altV with
                    //    | None -> None
                    //    | Some target -> 
                    //        let totalDamage =
                    //            List.init hitCount (fun _ -> 
                    //                let t = rand.UniformDouble()
                    //                damage.Lerp t
                    //            )
                    //            |> List.sum
                    //        let newHp = float target.currentHp - totalDamage
                    //        Some {target with currentHp = int newHp}
                    //)
                )
            (processed |> HashMap.ofList)
        let updateAmmo (currentAmmo : AmmunitionType) =
            match currentAmmo with
            | Endless -> Endless
            | Limited ammoInfo -> 
                let updatedAmmoInfo =
                    match ammoInfo.availableShots > 0 with
                    | true -> ammoInfo.availableShots - 1
                    | false -> 0
                       
                Limited {ammoInfo with availableShots = updatedAmmoInfo}


        {
            damage      = damage
            name        = "Shotgun"
            cooldown    = 0.5
            ammo        = Limited {
                                    reloadTime      = 5.0
                                    maxShots        = 2
                                    availableShots  = 2
                                  }
            range       = range
            spray       = 0.0
            canShoot    = canShoot
            createHitrays = createHitrays
            createShottrails = createShottrails
            findHitTargets = findHitTargets
            processHits = processHits
            updateAmmo = updateAmmo
        }
        //{
        //    damage      = Range1d(70,100)
        //    name        = "Shotgun"
        //    cooldown    = 1.5
        //    ammo        = Limited {
        //                            reloadTime      = 5.0
        //                            maxShots        = 2
        //                            availableShots  = 2
        //                          }
        //    range       = 10.0
        //    spray       = 10.0
        //}

    let scene (win : IRenderWindow) (activeWeapon : aval<WeaponType>) =
        
        let sigg =   
            win.Runtime.CreateFramebufferSignature([
                DefaultSemantic.Colors, TextureFormat.Rgba8; 
                DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
                ]
            )
            
        let gunProjection =
            win.Sizes 
            |> AVal.map (fun s -> 
                let aspect = float s.X / float s.Y
                Frustum.perspective 110.0 0.0001 20.0 aspect
                |> Frustum.projTrafo
            )
            
        let lg =
            Import.importGun("gun") 
            |> Sg.transform (
                    Trafo3d.Scale(1.0,1.0,-1.0) *
                    Trafo3d.Scale(0.25) *
                    Trafo3d.Translation(1.0,-1.0,-1.0))
        let sg = 
            Import.importGun("shotgun") 
            |> Sg.transform (
                    Trafo3d.Scale(1.0,1.0,-1.0) *
                    Trafo3d.Scale(0.18) *
                    Trafo3d.Translation(1.5,-1.0,-1.8))
        let task =  
            activeWeapon
            |> AVal.map (function 
                | Primary -> lg
                | Secondary -> sg
            )
            |> Sg.dynamic
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
                do! 
                    (fun (v : Effects.Vertex) -> 
                        fragment {
                            return V4d(v.c.X ** 0.5, v.c.Y ** 0.5, v.c.Z ** 0.5, 1.0)
                        }
                    )
                do! DefaultSurfaces.simpleLighting
            }
            |> Sg.viewTrafo (AVal.constant Trafo3d.Identity)
            |> Sg.projTrafo gunProjection
            //|> Sg.depthTest' DepthTest.None
            |> Sg.cullMode' CullMode.None
            |> Sg.fillMode' FillMode.Fill
            |> Sg.compile win.Runtime sigg
                
        let c : ClearValues =
            clear {
                color (C4b(0uy,0uy,0uy,0uy))
                depth 1.0
                stencil 0
            }
        let t = RenderTask.renderToColorWithClear win.Sizes c task 
        
        let gunSg =
            Sg.fullScreenQuad
            |> Sg.diffuseTexture t
            |> Sg.shader {
                do! DefaultSurfaces.diffuseTexture
            }
            |> Sg.blendMode' BlendMode.Blend
            |> Sg.depthTest' DepthTest.None
            |> Sg.pass Elm.Passes.pass1

        let crosshairSg =
            let shape =
                let t = 3.0
                let r = 30.0
                let w = 10.0
                let t2 = 4.0
                let color = C4b.Red 
                ShapeList.ofListWithRenderStyle RenderStyle.NoBoundary [
                    ConcreteShape.circle color t (Circle2d(V2d.Zero, r))
                    ConcreteShape.fillRectangle color (Box2d(r, -t/2.0, r + w, t/2.0))
                    ConcreteShape.fillRectangle color (Box2d(-r-w, -t/2.0, -r, t/2.0))
                    ConcreteShape.fillRectangle color (Box2d(-t/2.0, r, t/2.0, r + w))
                    ConcreteShape.fillRectangle color (Box2d(-t/2.0, -r - w, t/2.0, -r))
                    ConcreteShape.fillRectangle color (Box2d(-t2/2.0,-r,t2/2.0,r))
                    ConcreteShape.fillRectangle color (Box2d(-r,-t2/2.0,r,t2/2.0))
                ]
            Sg.shape (
                AVal.constant (
                    shape    
                )
            )
            |> Sg.viewTrafo' Trafo3d.Identity
            |> Sg.projTrafo (win.Sizes |> AVal.map (fun s -> Trafo3d.Scale(1.0 / float s.X, 1.0 / float s.Y, 1.0)))
        Sg.ofList [gunSg; crosshairSg]