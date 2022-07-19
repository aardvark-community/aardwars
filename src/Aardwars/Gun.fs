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
        startReloadTime : Option<float>
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
        canShoot         : AmmunitionType -> bool
        createHitrays    : CameraView -> list<Ray3d>
        createShottrails : float -> list<Ray3d> -> CameraView -> float -> list<TrailInfo>
        findHitTargets   : float -> list<Ray3d> -> HashMap<string, Target> ->  CameraView -> list<string>
        processHits      : list<string> -> HashMap<string, Target> -> HashMap<string, Target>
        updateAmmo       : AmmunitionType -> AmmunitionType
        startReload      : AmmunitionType -> float -> AmmunitionType
        reload           : AmmunitionType -> AmmunitionType
    }

module Weapon =
    let rand = RandomSystem()
    let canShoot (t : AmmunitionType) : bool = 
        match t with
        | Endless -> true
        | Limited ammoInfo -> ammoInfo.availableShots > 0 && ammoInfo.startReloadTime.IsNone

    let updateAmmo (currentAmmo : AmmunitionType) =
        match currentAmmo with
        | Endless -> Endless
        | Limited ammoInfo -> 
            let updatedAmmoInfo =
                match ammoInfo.availableShots > 0 with
                | true -> ammoInfo.availableShots - 1
                | false -> 0
                       
            Limited {ammoInfo with availableShots = updatedAmmoInfo}

    let reload (ammoType : AmmunitionType) =
        match ammoType with
        | Endless -> Endless
        | Limited ammoInfo -> Limited {ammoInfo with availableShots = ammoInfo.maxShots; startReloadTime = None}

    let startReload (ammoType : AmmunitionType) (startTime : float) =
        match ammoType with
        | Endless -> Endless
        | Limited ammoInfo -> Limited {ammoInfo with startReloadTime = Some startTime}

    let findHitTargets (range : float) (rays : list<Ray3d>)(targets : HashMap<string, Target>) (cv : CameraView) : list<string> =
        rays |> List.choose(fun shotRay -> 
            targets 
            |> HashMap.choose (fun name t -> 
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

    let laserGun =
        let damage = Range1d(10,20)
        let createHitrays (cv : CameraView) : list<Ray3d> = 
            let p = cv.Location
            let d = cv.Forward
            [Ray3d(p, d)]
        let createShottrails (range : float) (rays : list<Ray3d>) (cv : CameraView) time =
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

        {
            damage              = damage
            name                = "Lasergun"
            cooldown            = 0.5
            ammo                = AmmunitionType.Endless
            range               = 1000.0
            canShoot            = canShoot
            createHitrays       = createHitrays
            createShottrails    = createShottrails
            findHitTargets      = findHitTargets
            processHits         = processHits
            updateAmmo          = updateAmmo
            reload              = reload
            startReload         = startReload
        }

    let shotGun : Weapon =
        let damage = Range1d(5, 12)
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
        let createShottrails (range : float) (rays : list<Ray3d>) (cv : CameraView) time =
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
                )
            (processed |> HashMap.ofList)

        {
            damage               = damage
            name                 = "Shotgun"
            cooldown             = 0.5
            ammo                 = Limited {
                                             reloadTime      = 1.5
                                             maxShots        = 2
                                             availableShots  = 2
                                             startReloadTime = None
                                           }
            range               = 30.0
            canShoot            = canShoot
            createHitrays       = createHitrays
            createShottrails    = createShottrails
            findHitTargets      = findHitTargets
            processHits         = processHits
            updateAmmo          = updateAmmo
            reload              = reload
            startReload         = startReload
        }
        

    let scene 
        (emitGunT : V3d -> V3d -> unit)
        (win : IRenderWindow) 
        (activeWeapon : aval<WeaponType>) 
        (moveVec : aval<V3d>) 
        (fw : aval<V3d>)
        (dt : aval<float>)
        (gunT : aval<V3d>)
        (gunA : aval<V3d>)
        (gunLastFw : aval<V3d>) =
        
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
        let gunMotionTrafo =
            let lerpFactor = 20.999               
            AVal.custom (fun tok -> 
                let move = moveVec.GetValue tok
                let dt = dt.GetValue tok
                let fw = fw.GetValue tok
                let da = (dt * lerpFactor)
                let gunT = gunT.GetValue()
                let gunA = gunA.GetValue()
                let gunLastFw = gunLastFw.GetValue()

                let tx = 
                    let t = (move.X / 5.0) |> clamp -1.0 1.0
                    lerp gunT.X t da
                let ty = 
                    let t = (move.Y / 5.0) |> clamp -1.0 1.0
                    lerp gunT.Y t da
                    
                let ax = 
                    let v = clamp -1.0 1.0 (gunLastFw.X - fw.X)
                    lerp gunA.X v 0.08
                let ay = 
                    let v = clamp -1.0 1.0 (gunLastFw.Y - fw.Y)
                    lerp gunA.Y v 0.08

                emitGunT (V3d(tx,ty,gunT.Z)) (V3d(ax,ay,gunA.Z))
                Trafo3d.RotationZInDegrees(45.0 * ax) * 
                //Trafo3d.RotationXInDegrees(45.0 * ay) *
                Trafo3d.Translation(tx,0.0,ty)
            )
            
        let modelTrafo = 
            activeWeapon |> AVal.map (fun a -> 
                match a with 
                | Primary -> 
                    Trafo3d.Scale(1.0,1.0,-1.0) *
                    Trafo3d.Scale(0.25) *
                    Trafo3d.Translation(1.0,-1.0,-1.0)
                | Secondary -> 
                    Trafo3d.Scale(1.0,1.0,-1.0) *
                    Trafo3d.Scale(0.18) *
                    Trafo3d.Translation(1.5,-1.0,-1.8)
                    
            )

        let lg =
            Import.importGun("gun") 
        let sg = 
            Import.importGun("shotgun") 
        let task =  
            activeWeapon
            |> AVal.map (function 
                | Primary -> lg
                | Secondary -> sg
            )
            |> Sg.dynamic
            |> Sg.trafo gunMotionTrafo
            |> Sg.trafo modelTrafo
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
            |> Sg.pass Elm.Passes.pass2

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