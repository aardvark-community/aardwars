namespace Aardwars

open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Rendering.Text
open System
open Aardvark.SceneGraph.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open FShade

type StartReloadTimeInfo =
    | Not
    | PausedReload of float
    | Reloading of float
    with member x.NotReloading = match x with Not -> true | _ -> false

type AmmoInfo = 
    {
        reloadTime : float
        startReloadTime : StartReloadTimeInfo
        maxShots : int
        availableShots : int
    }

type AmmunitionType = 
    | Endless
    | Limited of AmmoInfo
module AmmunitionType =
    let reload (ammoType : AmmunitionType) =
        match ammoType with
        | Endless -> Endless
        | Limited ammoInfo -> Limited {ammoInfo with availableShots = ammoInfo.maxShots; startReloadTime = Not}

    let startReload (ammoType : AmmunitionType) (startTime : float) =
        match ammoType with
        | Endless -> Endless
        | Limited ammoInfo -> 
            if ammoInfo.availableShots < ammoInfo.maxShots then 
                match ammoInfo.startReloadTime with 
                | Not -> Limited {ammoInfo with startReloadTime = Reloading startTime}
                | _ -> Limited ammoInfo
            else Limited ammoInfo
    
    let isReloading (a : AmmunitionType) =
        match a with
        | Limited ammo -> 
            match ammo.startReloadTime with 
            | Not -> false
            | _ -> true
        | _ -> false
    let isEmpty (a : AmmunitionType) =
        match a with
        | Limited ammo -> ammo.availableShots <= 0
        | _ -> false

type WeaponType =
    | LaserGun
    | Shotgun
    | Sniper
    | RocketLauncher
    | RainbowGun
    
module WeaponType =
    let pickle (t : WeaponType) =
        match t with
        | LaserGun        -> 0
        | Shotgun         -> 1
        | Sniper          -> 2
        | RocketLauncher  -> 3
        | RainbowGun      -> 4
    let unpickle (t : int) =
        match t with
        | 0 -> LaserGun       
        | 1 -> Shotgun        
        | 2 -> Sniper         
        | 3 -> RocketLauncher 
        | 4 -> RainbowGun   
        | _ -> failwith "ashudfchza"
    let toString (w : WeaponType) =
        match w with 
        | LaserGun -> "Laser Gun"
        | Shotgun -> "Shotgun"
        | RocketLauncher -> "Rocket Launcher"
        | RainbowGun -> "Rainbow Gun"
        | Sniper -> "Sniper Rifle"


type TrailInfo = 
    {
        color       :  C4b
        realLine    :  Line3d
        offsetLine  :  Line3d
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
    
type OtherPlayerInfo =
    {
        color : string
        pos : V3d
        fw : V3d
        weapon : WeaponType
        reloading : bool
        frags : int
        deaths : int
    }
    
type ProjectileCreationInfo =
    {
        pos : V3d
        vel : V3d
        smallRadius : float
        smallDmg : float
        bigRadius : float
        bigDmg : float
    }
type Weapon =
    {
        damage           : Range1d
        name             : string
        cooldown         : float
        ammo             : AmmunitionType
        range            : float
        canShoot         : AmmunitionType -> Option<float> -> float -> float -> bool
        createHitrays    : CameraView -> list<Ray3d>
        createProjectiles : CameraView -> list<ProjectileCreationInfo>
        createShottrails : float -> list<Ray3d> -> CameraView -> float -> list<TrailInfo>
        //findHitTargets   : float -> list<Ray3d> -> HashMap<string, Target> -> HashMap<string, OtherPlayerInfo> ->  CameraView -> list<string> * list<string>
        processHits      : list<string> -> HashMap<string, Target> -> HashMap<string, Target>
        calculateDamage  : int -> int
        updateAmmo       : AmmunitionType -> AmmunitionType
        startReload      : AmmunitionType -> float -> AmmunitionType
        reload           : AmmunitionType -> AmmunitionType
        lastShotTime     : Option<float>
        waitTimeBetweenShots : float
    }

module Weapon =
    let isReloading (w : Weapon) = w.ammo |> AmmunitionType.isReloading
    let rand = RandomSystem()
    let random = System.Random ()
    let canShoot (t : AmmunitionType) (lastShotTime : Option<float>) (waitTimeBetweenShots : float) (currentTime : float) : bool = 
        let cooldownFinished = 
            match lastShotTime with
            | None -> true
            | Some lastShotTime -> lastShotTime + waitTimeBetweenShots <= currentTime

        match t with
        | Endless -> cooldownFinished
        | Limited ammoInfo -> 
            ammoInfo.availableShots > 0 && ammoInfo.startReloadTime.NotReloading && cooldownFinished


    let updateAmmo (currentAmmo : AmmunitionType) =
        match currentAmmo with
        | Endless -> Endless
        | Limited ammoInfo -> 
            let updatedAmmoInfo =
                match ammoInfo.availableShots > 0 with
                | true -> ammoInfo.availableShots - 1
                | false -> 0
                       
            Limited {ammoInfo with availableShots = updatedAmmoInfo}

    
    //let cooldownFinished (lastShotTime : float) (waitTimeBetweenShots : float) (currentTime : float) : bool =
    //    lastShotTime + waitTimeBetweenShots <= currentTime
    
    


    let findHitTargets (range : float) (rays : list<Ray3d>) (world : World) (targets : HashMap<string, Target>) (otherPlayers : HashMap<string, OtherPlayerInfo>) (cv : CameraView) =
        
        let mutable floorHits = []
        
        let rays = rays |> List.indexed
        let rays =
            rays |> List.map (fun (i,r) ->
                match world.Intersections r 0.0 range |> Seq.tryHead with
                | Some (t,_) -> 
                    floorHits <- (i,r.GetPointOnRay (t)) :: floorHits
                    (i, r, min t range)
                | _ -> 
                    (i, r, range)
            )
        let hitTargets =
            rays |> List.choose(fun (i, shotRay, range) -> 
                targets 
                |> HashMap.choose (fun name t -> 
                    let d0 = t.pos -  cv.Location
                    let d1 = cv.Forward
                    let inFront = (Vec.dot d0.Normalized d1.Normalized) > 0.0
                    let mutable tout = 0.0
                    let isHit = shotRay.HitsSphere(t.pos,t.radius,&tout)
                    let isHit = isHit && tout <= range
                    match isHit && inFront with
                    |true -> Some (tout,shotRay.GetPointOnRay tout)
                    |false -> None
                ) 
                |> HashMap.toList
                |> List.sortBy (fun (_,(t,_)) -> t)
                |> List.tryHead
                |> Option.map (fun (name,(_,p)) -> i,name,p)
            )

        let hitPlayers =
            rays |> List.choose(fun (i, shotRay, range) -> 
                otherPlayers |> HashMap.toList |> List.choose (fun (name,info) ->
                    let pos = info.pos
                    let b = playerBounds.Translated(pos)
                    let mutable t = 0.0
                    if b.Intersects(shotRay, &t) && t >= 0.0 && t <= range then Some (name,t,shotRay.GetPointOnRay t)
                    else None
                )
                |> List.sortBy (fun (_,t,_) -> t)
                |> List.tryHead
                |> Option.map (fun (n,_,p) -> i,n,p)
            )
        hitTargets,hitPlayers,floorHits

    let laserGun =
        let damage = Range1d(10,15)
        let spread = 0.01075
        let createHitrays (cv : CameraView) : list<Ray3d> = 
            let u = (rand.UniformDouble() * 2.0 - 1.0) * spread
            let v = (rand.UniformDouble() * 2.0 - 1.0) * spread
            let p = cv.Location
            let p0 = cv.Location + cv.Forward
            let p' = p0 + u * cv.Right + v * cv.Up
            let d' = p' - p
            [Ray3d(p, d'.Normalized)]
        let createShottrails (range : float) (rays : list<Ray3d>) (cv : CameraView) time =
            rays |> List.map(fun shotRay ->
                let p0 = shotRay.Origin + cv.Right * 0.7 + cv.Down * 0.3 + cv.Forward * 1.0
                
                let p1 = shotRay.Origin + range * shotRay.Direction
                let rline = Line3d(shotRay.Origin, p1)
                let oline = Line3d(p0, p1)
                {
                    color = C4b.Beige
                    realLine = rline
                    offsetLine = oline
                    startTime = time
                    duration = 1.0
                }
            )
        let calculateDamage (hitCount : int) : int =                        
            List.init hitCount (fun _ -> 
                let t = rand.UniformDouble()
                damage.Lerp t
            )
            |> List.sum
            |> int
            
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
                            let newHp =  target.currentHp - (calculateDamage hitCount)
                            Some {target with currentHp = int newHp}
                    )
                )
            (processed |> HashMap.unionMany)
        {
            damage              = damage
            name                = "Lasergun"
            cooldown            = 0.25
            ammo                = AmmunitionType.Endless
            range               = 1000.0
            canShoot            = canShoot
            createHitrays       = createHitrays
            createProjectiles   = fun _ -> List.empty
            createShottrails    = createShottrails
            calculateDamage     = calculateDamage
            processHits         = processHits
            updateAmmo          = updateAmmo
            reload              = AmmunitionType.reload
            startReload         = AmmunitionType.startReload
            lastShotTime        = None
            waitTimeBetweenShots = 0.0
        }

    let shotGun : Weapon =
        let damage = Range1d(8.75, 8.75)
        let spread = 0.125
        let createHitrays (cv : CameraView) : list<Ray3d> = 
            List.init 32 (fun _ ->
                let u = (rand.UniformDouble() * 2.0 - 1.0) * spread
                let v = (rand.UniformDouble() * 2.0 - 1.0) * spread
                let p = cv.Location
                let p0 = cv.Location + cv.Forward
                let p' = p0 + u * cv.Right + v * cv.Up
                let d' = p' - p
                Ray3d(p, d'.Normalized)
            )
        let createShottrails (range : float) (rays : list<Ray3d>) (cv : CameraView) time =
            rays |> List.map(fun shotRay ->
                let p0 = shotRay.Origin + cv.Right * 0.3 + cv.Down * 0.15 + cv.Forward * 0.6
                
                let p1 = shotRay.Origin + range * shotRay.Direction
                let rline = Line3d(shotRay.Origin, p1)
                let oline = Line3d(p0, p1)
                {
                    color = C4b.Beige
                    realLine = rline
                    offsetLine = oline
                    startTime = time
                    duration = 0.5
                }
            )
        let calculateDamage (hitCount : int) : int =                        
            List.init hitCount (fun _ -> 
                let t = rand.UniformDouble()
                damage.Lerp t
            )
            |> List.sum
            |> int

        let processHits (names : list<string>) (targets : HashMap<string, Target>) : HashMap<string, Target> =
            let processed = 
                names 
                |> List.groupBy id
                |> List.map (fun (s,ss) -> 
                    let hitCount = ss |> List.length
                    let target = targets.Item s
                    let newHp = target.currentHp - (calculateDamage hitCount)
                    s, {target with currentHp = int newHp}
                )
            (processed |> HashMap.ofList)

        {
            damage               = damage
            name                 = "Shotgun"
            cooldown             = 0.5
            ammo                 = Limited {
                                             reloadTime      = 1.45
                                             maxShots        = 2
                                             availableShots  = 2
                                             startReloadTime = Not
                                           }
            range               = 25.0
            canShoot            = canShoot
            createHitrays       = createHitrays
            createProjectiles   = fun _ -> List.empty
            createShottrails    = createShottrails
            processHits         = processHits
            updateAmmo          = updateAmmo
            calculateDamage     = calculateDamage
            reload              = AmmunitionType.reload
            startReload         = AmmunitionType.startReload
            lastShotTime        = None
            waitTimeBetweenShots = 0.2
        }

    let sniper : Weapon =
        let damage = Range1d(90, 92)
        let createHitrays (cv : CameraView) : list<Ray3d> = 
            let p = cv.Location
            let d = cv.Forward
            [Ray3d(p, d)]
        let createShottrails (range : float) (rays : list<Ray3d>) (cv : CameraView) time =
            rays |> List.map(fun shotRay ->
                let p0 = shotRay.Origin + cv.Right * 0.7 + cv.Down * 0.4 + cv.Forward * 1.0
                
                let p1 = shotRay.Origin + range * shotRay.Direction
                let rline = Line3d(shotRay.Origin, p1)
                let oline = Line3d(p0, p1)
                {
                    color = C4b.Beige
                    realLine = rline
                    offsetLine = oline
                    startTime = time
                    duration = 2.5
                }
            )
        let calculateDamage (hitCount : int) : int =                        
            List.init hitCount (fun _ -> 
                let t = rand.UniformDouble()
                damage.Lerp t
            )
            |> List.sum
            |> int

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
                            let newHp =  target.currentHp - (calculateDamage hitCount)
                            Some {target with currentHp = int newHp}
                    )
                )
            (processed |> HashMap.unionMany)

        {
            damage               = damage
            name                 = "Sniper"
            cooldown             = 0.0
            ammo                 = Limited {
                                                reloadTime      = 2.35
                                                maxShots        = 3
                                                availableShots  = 3
                                                startReloadTime = Not
                                            }
            range               = 1000.0
            canShoot            = canShoot
            createHitrays       = createHitrays
            createShottrails    = createShottrails
            processHits         = processHits
            createProjectiles = fun _ -> []
            updateAmmo          = updateAmmo
            calculateDamage     = calculateDamage
            reload              = AmmunitionType.reload
            startReload         = AmmunitionType.startReload
            lastShotTime        = None
            waitTimeBetweenShots = 1.0
        }

    let rainbowgun : Weapon =
            let damage = Range1d(19, 22)
            let spread = 0.0335
            let createHitrays (cv : CameraView) : list<Ray3d> = 
                let u = (rand.UniformDouble() * 2.0 - 1.0) * spread
                let v = (rand.UniformDouble() * 2.0 - 1.0) * spread
                let p = cv.Location
                let p0 = cv.Location + cv.Forward
                let p' = p0 + u * cv.Right + v * cv.Up
                let d' = p' - p
                [Ray3d(p, d'.Normalized)]
            let createShottrails (range : float) (rays : list<Ray3d>) (cv : CameraView) time =
                rays |> List.map(fun shotRay ->
                    let p0 = shotRay.Origin + cv.Right * 0.7 + cv.Down * 0.4 + cv.Forward * 1.0
                    let p1 = shotRay.Origin + range * shotRay.Direction
                    let rline = Line3d(shotRay.Origin, p1)
                    let oline = Line3d(p0, p1)
                    let color =
                        let h = (rand.UniformDouble())
                        HSVf(h,1.0,1.0).ToC3f().ToC4b()
                    {
                        color = color
                        realLine = rline
                        offsetLine = oline
                        startTime = time
                        duration = 1.0
                    }
                )
            let calculateDamage (hitCount : int) : int =                        
                List.init hitCount (fun _ -> 
                    let t = rand.UniformDouble()
                    damage.Lerp t
                )
                |> List.sum
                |> int

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
                                let newHp =  target.currentHp - (calculateDamage hitCount)
                                Some {target with currentHp = int newHp}
                        )
                    )
                (processed |> HashMap.unionMany)

            {
                damage               = damage
                name                 = "Rainbowgun"
                cooldown             = 0.0
                ammo                 = Limited {
                                                 reloadTime      = 1.85
                                                 maxShots        = 30
                                                 availableShots  = 30
                                                 startReloadTime = Not
                                               }
                range               = 100.0
                canShoot            = canShoot
                createHitrays       = createHitrays
                createProjectiles   = fun _ -> List.empty
                createShottrails    = createShottrails
                processHits         = processHits
                updateAmmo          = updateAmmo
                calculateDamage     = calculateDamage
                reload              = AmmunitionType.reload
                startReload         = AmmunitionType.startReload
                lastShotTime        = None
                waitTimeBetweenShots = 0.15

            }


    let rocketLauncher =
        let createProjectiles (cv : CameraView) =
            let pos = cv.Location + 0.5 * cv.Forward
            let vel = cv.Forward * 30.5
            let smallRadius = 0.95
            let smallDmg = 25.0
            let bigRadius = 2.75
            let bigDmg = 40.0
            [
                {
                    pos             = pos
                    vel             = vel
                    smallRadius     = smallRadius
                    smallDmg        = smallDmg
                    bigRadius       = bigRadius
                    bigDmg          = bigDmg
                }
            ]
        
        {
            damage               = Range1d.Infinite
            name                 = "Rocket Launcher"
            cooldown             = 0.05
            ammo                 = Limited {
                                                reloadTime      = 1.95
                                                maxShots        = 5
                                                availableShots  = 5
                                                startReloadTime = Not
                                            }
            range               = 12345.0
            canShoot            = canShoot
            createHitrays       = fun _ -> List.empty
            createProjectiles   = createProjectiles
            createShottrails    = fun _ _ _ _ -> List.empty
            processHits         = fun _ _ -> HashMap.empty
            updateAmmo          = updateAmmo
            calculateDamage     = fun _ -> 0
            reload              = AmmunitionType.reload
            startReload         = AmmunitionType.startReload
            lastShotTime        = None
            waitTimeBetweenShots = 0.05
        }
    let lgTex =
        Import.loadTexture(@"assets/gun.png")
    let sgTex =
        Import.loadTexture(@"assets/Shotgun.png")
    let snTex =
        Import.loadTexture(@"assets/sniper.png")
    let rgTex =
        Import.loadTexture(@"assets/rainbowgun.png")
    let rlTex =
        Import.loadTexture(@"assets/rocketlauncher.png")
    let lg =
        lazy Import.importGun("gun") 
    let sg = 
        lazy Import.importGun("shotgun")
    let sn =
        lazy Import.importGun("sniper")
    let rg =
        lazy Import.importGun("rainbowgun")
    let rl = 
        lazy Import.importGun("rocketlauncher")

    let gunModel (activeWeapon : aval<WeaponType>) (isReloading : aval<bool>) (gunMotionTrafo : aval<Trafo3d>)=
        let modelTrafo = 
            activeWeapon |> AVal.map (fun a -> 
                match a with 
                | LaserGun -> 
                    Trafo3d.Scale(1.0,1.0,-1.0) *
                    Trafo3d.Scale(0.25) *
                    Trafo3d.Translation(1.0,-1.0,-1.0)
                | Shotgun -> 
                    Trafo3d.Scale(1.0,1.0,-1.0) *
                    Trafo3d.Scale(0.18) *
                    Trafo3d.Translation(1.5,-1.0,-1.8)
                | Sniper ->
                    Trafo3d.Scale(1.0,1.0,1.0) *
                    Trafo3d.Scale(0.3) *
                    Trafo3d.Translation(1.5,-1.0,-2.0)
                | RocketLauncher -> 
                    //Trafo3d.RotationY(-1.5707963268) *
                    Trafo3d.Scale(1.0,1.0,1.0) *
                    Trafo3d.Scale(0.3) *
                    Trafo3d.Translation(1.5,-1.0,-2.25)  
                | RainbowGun ->
                    Trafo3d.Scale(1.0,1.0,1.0) *
                    Trafo3d.Scale(0.3) *
                    Trafo3d.Translation(1.15,-1.0,-1.5)
            )

        let modelSurface =
            Sg.adapter
            >> Sg.shader {
                do! DefaultSurfaces.trafo
                do! Shader.guntexy
                do! 
                    (fun (v : Effects.Vertex) -> 
                        fragment {
                            let d : float32 = uniform?Dark
                            if d > 0.5f then return V4d(v.c.X ** 2.0, v.c.Y ** 2.0, v.c.Z ** 2.0, 1.0)
                            else return V4d(v.c.X ** 0.5, v.c.Y ** 0.5, v.c.Z ** 0.5, 1.0)
                        }
                    )
                do! DefaultSurfaces.simpleLighting
            }
            >> Sg.uniform "Dark" (isReloading |> AVal.map (fun r -> if r then 1.0f else 0.0f))
                
        //let tex = 
        //    activeWeapon |> AVal.map (function 
        //    | LaserGun ->       lgTex
        //    | Shotgun ->        sgTex
        //    | Sniper ->         snTex
        //    | RainbowGun ->     rgTex
        //    | RocketLauncher -> rlTex
        //)

        let selectedGunTex =
            activeWeapon
            |> AVal.map (function 
                | LaserGun ->       0.0f
                | Shotgun ->        1.0f
                | Sniper ->         2.0f
                | RainbowGun ->     3.0f
                | RocketLauncher -> 4.0f
            )

        activeWeapon
        |> AVal.map (function 
            | LaserGun ->       lg.Value |> modelSurface 
            | Shotgun ->        sg.Value |> modelSurface
            | Sniper ->         sn.Value |> modelSurface
            | RainbowGun ->     rg.Value |> modelSurface
            | RocketLauncher -> rl.Value |> modelSurface
        )
        |> Sg.dynamic
        |> Sg.uniform "GunTex" selectedGunTex
        |> Sg.texture' "laserguntex" lgTex
        |> Sg.texture' "shotguntex" sgTex
        |> Sg.texture' "snipertex" snTex
        |> Sg.texture' "rainbowguntex" rgTex
        |> Sg.texture' "rocketlaunchertex" rlTex
        //Sg.ofList [
        //    lg.Value |> modelSurface |> Sg.onOff (activeWeapon |> AVal.map (fun aw -> aw=LaserGun      ))
        //    sg.Value |> modelSurface |> Sg.onOff (activeWeapon |> AVal.map (fun aw -> aw=Shotgun       ))
        //    sn.Value |> modelSurface |> Sg.onOff (activeWeapon |> AVal.map (fun aw -> aw=Sniper        ))
        //    rg.Value |> modelSurface |> Sg.onOff (activeWeapon |> AVal.map (fun aw -> aw=RainbowGun    ))
        //    rl.Value |> modelSurface |> Sg.onOff (activeWeapon |> AVal.map (fun aw -> aw=RocketLauncher))
        //]
        |> Sg.trafo gunMotionTrafo
        |> Sg.trafo modelTrafo

    let scene 
        (emitGunT : V3d -> V3d -> unit)
        (win : IRenderWindow) 
        (activeWeapon : aval<WeaponType>) 
        (moveVec : aval<V3d>) 
        (fw : aval<V3d>)
        (dt : aval<float>)
        (gunT : aval<V3d>)
        (gunA : aval<V3d>)
        (gunLastFw : aval<V3d>)
        (isReloading : aval<bool>) =
        
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
            let translationLerpFactor = 20.999      
            let angleLerpFactor = 10.0        
            AVal.custom (fun tok -> 
                let move = moveVec.GetValue tok
                let dt = dt.GetValue tok
                let fw = fw.GetValue tok
                let gunT = gunT.GetValue()
                let gunA = gunA.GetValue()
                let gunLastFw = gunLastFw.GetValue()

                let da = dt * translationLerpFactor
                let tx = 
                    let t = (move.X / 5.0) |> clamp -1.0 1.0
                    lerp gunT.X t da
                let ty = 
                    let t = (move.Y / 5.0) |> clamp -1.0 1.0
                    lerp gunT.Y t da

                let da = dt * angleLerpFactor
                let ax = 
                    let a = gunLastFw.XY
                    let b = fw.XY
                    let v = 
                        if Vec.AngleBetween(a,b) < 0.01 then 0.0
                        else clamp -1.0 1.0 ((atan2 (a.X*b.Y - a.Y*b.X) (a.X*b.X + a.Y*b.Y)) / Constant.Pi)
                    lerp gunA.X v da
                let inline anpassen a =
                    clamp -180.0 180.0 (-((a) * 720.0))

                emitGunT (V3d(tx,ty,gunT.Z)) (V3d(ax,gunA.Y,gunA.Z))
                Trafo3d.RotationYInDegrees(anpassen ax) * 
                Trafo3d.Translation(tx,0.0,ty)
            )
            
        let task = 
            gunModel activeWeapon isReloading gunMotionTrafo
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
                let color = C4b.LightSeaGreen
                ShapeList.ofListWithRenderStyle RenderStyle.NoBoundary [
                    //ConcreteShape.circle color t (Circle2d(V2d.Zero, r))
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