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



type Message =
    | MouseMove of delta : V2d
    | MouseDown of button : MouseButtons
    | MouseUp of button : MouseButtons
    | MouseScroll of scroll : float
    | KeyDown of key : Keys
    | KeyUp of key : Keys
    | Resize of newSize : V2i
    | UpdateTime of seconds : float * delta : float
    | UpdateAnimationState of AnimationState
    | SpawnShotTrails of list<Line3d*float*C4b>
    | SpawnProjectiles of list<ProjectileInfo>
    | UpdateProjectiles of dt:float
    | Explode of ExplosionInfo
    | CreateExplosionAnimation of ExplosionAnimationInfo
    | Shoot
    | UpdatePlayerPos of string * pos:V3d*fw:V3d*w:int*rld:bool
    | HitBy of string * float * V3d * WeaponType
    | HitByWithSlap of string * float * V3d * V3d * WeaponType
    | UpdateStats of Map<string,(int*int*string)>
    | EnemyDied of killer:string*died:string*gun:WeaponType
    | KillfeedMessage of killer:string*died:string*gun:WeaponType
    | HitEnemy of enemyName:string * damage:float * sourcePos:V3d * w:WeaponType
    | HitEnemyWithSlap of enemyName:string * damage:float * vel:V3d * sourcePos:V3d * w:WeaponType
    | CreateGotHitIndicatorInstance of sourcePos:V3d*dmg:float
    | CreateHitEnemyIndicatorInstance of name:string
    | TeleportToSpawnLocation
    | RestartGame of force:bool
    | Respawn of force:bool
    | ResetPlayerState
    | ResetWorldState
    | Disconnected of string
    | SyncRestartTime of float
    | SyncServerTime of float
    

module Update =
    let rand = RandomSystem()
    let random = System.Random()

    
    let events (client : NetworkClient) (env : Environment<Message>) =
        let win = env.Window
        let glfw = win.GetType().GetField("glfw", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> Silk.NET.GLFW.Glfw
        let hwin = win.GetType().GetField("win", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> nativeptr<Silk.NET.GLFW.WindowHandle>
        let mutable mouseDelta = V2d.Zero
        win.Cursor <- Cursor.None
    
        client.receive.Add (fun msg ->
            match msg with
            | NetworkMessage.UpdatePosition(player,pos,fw,w,rld) ->
                env.Emit [UpdatePlayerPos(player,pos,fw,w,rld)]
            | NetworkMessage.Hit(player, dmg, sd,w) ->
                env.Emit [HitBy(player, dmg, sd, WeaponType.unpickle w)]
            | NetworkMessage.HitWithSlap(player, dmg, vel, sd,w) ->
                env.Emit [HitByWithSlap(player, dmg, vel, sd, WeaponType.unpickle w)]
            | NetworkMessage.Stats(s,t,st) ->
                env.Emit [UpdateStats s; SyncRestartTime t; SyncServerTime st]
            | NetworkMessage.SpawnShotTrails trails -> 
                env.Emit [SpawnShotTrails (trails |> List.map (fun (l,s,d,c) -> l,d,c))]
            | NetworkMessage.Died (k,d,w) -> 
                env.Emit [EnemyDied (k,d,WeaponType.unpickle w)]
            | NetworkMessage.Disconnected n -> 
                env.Emit [Disconnected n]
            | NetworkMessage.Restart newTime -> 
                env.Emit [RestartGame true; SyncRestartTime newTime]
            | NetworkMessage.SyncRestartTime t -> 
                env.Emit [SyncRestartTime t]
            | NetworkMessage.SpawnProjectiles projs ->
                let projs = 
                    projs |> List.map (fun (n,p,v,d,sr,br,sd,bd) -> 
                        {
                            Owner                   = n
                            Position                = p
                            Velocity                = v
                            StartTime               = 0.0
                            MaxDuration             = d
                            ExplosionSmallRadius    = sr
                            ExplosionBigRadius      = br
                            ExplosionSmallDamage    = sd
                            ExplosionBigDamage      = bd
                            Trail                   = []
                        }
                    )
                env.Emit [SpawnProjectiles projs]
            | NetworkMessage.Explode (o,p,sr,br,sd,bd) -> 
                env.Emit [Explode {Owner=o;Position=p;SmallRadius=sr;BigRadius=br;SmallDamage=sd;BigDamage=bd}]
            | _ ->
                printfn "%A" msg
        )   

        win.Mouse.Move.Values.Add(fun (o,n) ->
            let c = V2d (AVal.force win.Sizes) / 2.0
            let mutable px = 0.0
            let mutable py = 0.0
            glfw.GetCursorPos(hwin, &px, &py)
            glfw.SetCursorPos(hwin, c.X, c.Y)
            env.Emit [ MouseMove (V2d(px, py) - c) ]
        )

        win.Mouse.Down.Values.Add(fun b -> 
            env.Emit [ MouseDown b ]
        )

        win.Mouse.Up.Values.Add(fun b -> 
            env.Emit [ MouseUp b ]
        )

        win.Mouse.Scroll.Values.Add(fun b ->
            env.Emit [ MouseScroll b ]
        )

        let sw = System.Diagnostics.Stopwatch.StartNew()
        let mutable last = sw.Elapsed.TotalSeconds
        let timer = 
            env.StartTimer(16, fun () ->
                let now = sw.Elapsed.TotalSeconds
                env.Emit [UpdateTime(now, now - last)]
                last <- now
            )

        env.Window.Keyboard.Down.Values.Add(fun k ->
            env.Emit [KeyDown k]
        )
        env.Window.Keyboard.Up.Values.Add(fun k ->
            env.Emit [KeyUp k]
        )

        env.Window.Sizes.AddCallback (fun s ->
            env.Emit [Resize s]
        ) |> ignore

    let update (client : NetworkClient) (env : Environment<Message>) (model : Model) (message : Message) =
        let inline cam (msg : CameraMessage) (m : Model) =
            let p0 = m.camera.camera.Location
            let newCam = CameraController.update model.camera msg
            let p1 = newCam.camera.Location
            let newModel = 
                if p0 <> p1 then
                    let (p1, floor) = m.world.Hit p0 p1
                    let newCam = { newCam with camera = newCam.camera.WithLocation(p1) }
                    { m with camera = newCam; onFloor = floor }
                else    
                    { m with camera = newCam }
            if newModel.onFloor then   
                { newModel with 
                    camera = 
                        { newModel.camera with 
                            velocity = 
                                if Model.controlsDisabled m then V3d.Zero
                                else 
                                    let vn = Fun.Lerp(0.1, newModel.camera.velocity, newModel.camera.move)
                                    V3d(vn.X,vn.Y,newModel.camera.move.Z)
                        } 
                }
            else
                { newModel with 
                    camera = 
                        { newModel.camera with 
                            velocity =      
                                if Model.controlsDisabled m then V3d.Zero
                                else 
                                    let x = model.airAccel * newModel.camera.move.X + newModel.camera.velocity.X
                                    let y = model.airAccel * newModel.camera.move.Y + newModel.camera.velocity.Y
                                    V3d(x,y,newModel.camera.velocity.Z)
                        } 
                }

        match message with
        | Disconnected n -> 
            {model with otherPlayers = model.otherPlayers |> HashMap.remove n}
        | HitBy(player, dmg, sd, w) ->
            if (Model.amDead model) then model 
            else
                let hp = model.currentHp - dmg
                if hp <= 0.0 then   
                    let model = {model with lastGotHit=Some (player,w)}
                    client.send (NetworkCommand.Died(player,model.playerName,w |> WeaponType.pickle))
                    env.Emit [KillfeedMessage(player,model.playerName,w)]
                    {model with deathTime=Some model.time;camera={model.camera with blastVelocity=V3d.Zero}}
                else
                    env.Emit [CreateGotHitIndicatorInstance(sd,dmg)]
                    {model with currentHp = hp}
        | HitByWithSlap(player, dmg, vel, sd,w) ->
            env.Emit [HitBy(player,dmg, sd,w)]
            {model with 
                camera =
                    {model.camera with blastVelocity = model.camera.blastVelocity + vel}
            }
        | UpdatePlayerPos(player, pos, fw, w, rld) ->
            let w = WeaponType.unpickle w
            { model with otherPlayers = HashMap.alter player (function Some o -> Some {o with pos=pos;fw=fw;weapon=w} | None -> Some {pos=pos;fw=fw;reloading=rld;weapon=w;frags=0;deaths=0;color="yellow"}) model.otherPlayers }
        | MouseMove delta -> model |> cam (CameraMessage.Look (delta,model.isZoomed))
        | KeyDown Keys.W -> model |> cam (CameraMessage.StartMove (V3d(0.0, model.moveSpeed, 0.0)))
        | KeyUp Keys.W -> model |> cam (CameraMessage.StopMove (V3d(0.0, model.moveSpeed, 0.0)))
        | KeyDown Keys.S -> model |> cam (CameraMessage.StartMove (V3d(0.0, -model.moveSpeed, 0.0)))
        | KeyUp Keys.S -> model |> cam (CameraMessage.StopMove (V3d(0.0, -model.moveSpeed, 0.0)))
        | KeyDown Keys.A  -> model |> cam (CameraMessage.StartMove (V3d(-model.moveSpeed, 0.0, 0.0)))
        | KeyUp Keys.A -> model |> cam (CameraMessage.StopMove (V3d(-model.moveSpeed, 0.0, 0.0)))
        | KeyDown Keys.D -> model |> cam (CameraMessage.StartMove (V3d(model.moveSpeed, 0.0, 0.0)))
        | KeyUp Keys.D -> model |> cam (CameraMessage.StopMove (V3d(model.moveSpeed, 0.0, 0.0)))
        | KeyDown Keys.Space -> 
            if (Model.amDead model) then 
                env.Emit [Respawn false]
                model
            else model |> cam (CameraMessage.StartMove (V3d(0.0, 0.0, 10.0)))
        | KeyUp Keys.Space -> model
        | KeyDown Keys.Tab -> {model with tabDown=true}
        | KeyUp Keys.Tab -> {model with tabDown=false}
        | KeyDown Keys.LeftCtrl | KeyDown Keys.RightCtrl -> {model with ctrlDown=true}
        | KeyUp Keys.LeftCtrl | KeyUp Keys.RightCtrl -> {model with ctrlDown=false}
        | KeyDown Keys.LeftShift | KeyDown Keys.RightShift -> {model with shiftDown=true}
        | KeyUp Keys.LeftShift | KeyUp Keys.RightShift -> {model with shiftDown=false}
        | KeyDown Keys.D1 -> {model with activeWeapon = LaserGun}
        | KeyDown Keys.D2 | KeyDown Keys.F -> {model with activeWeapon = Shotgun}
        | KeyDown Keys.D3 | KeyDown Keys.Q -> {model with activeWeapon = Sniper}
        | KeyDown Keys.D4 | KeyDown Keys.C -> {model with activeWeapon = RainbowGun}
        | KeyDown Keys.D5 | KeyDown Keys.G -> {model with activeWeapon = RocketLauncher}
        | KeyDown Keys.P -> 
            if model.ctrlDown && model.shiftDown then 
                env.Emit [RestartGame false]
                client.send (NetworkCommand.Restart)
                model
            else
                printfn "position: %A" model.camera.camera.Location
                model
        | KeyDown Keys.R ->
            if (Model.controlsDisabled model) then model
            else
                let weapon = model.weapons.Item model.activeWeapon
                let updatedWeapons = 
                    model.weapons 
                    |> HashMap.add model.activeWeapon {weapon with ammo = (weapon.startReload weapon.ammo model.time)}
                {model with weapons = updatedWeapons}
        | KeyDown Keys.Back -> 
            if (Model.amDead model) then 
                env.Emit [Respawn false]
                model
            elif model.lastPositionReset + 5.0 < model.time then 
                env.Emit [TeleportToSpawnLocation]
                {model with lastPositionReset = model.time}
            else model
        | Resize s -> 
            { model with 
                size = s
                proj = Frustum.perspective 110.0 0.1 1000.0 (float s.X / float s.Y) 
            }
        | SyncRestartTime t -> 
            {model with gameStartTime = t}
        | SyncServerTime st -> 
            {model with serverTime = st}
        | UpdateStats s -> 
            let (myKills,myDeaths,myColor) = s |> Map.tryFind model.playerName |> Option.defaultValue (0,0,"yellow")
            let newOthers = 
                (model.otherPlayers, s) ||> Map.fold (fun others name (kills,deaths,color) -> others |> HashMap.update name (function Some o -> {o with frags=kills;deaths=deaths;color=color}|None -> {pos=V3d(234234234.0,346346346.0,35757457.0);fw=V3d.IOO;reloading=false;weapon=WeaponType.LaserGun;frags=kills;deaths=deaths;color=color}))
                |> HashMap.remove model.playerName
            {model with frags=myKills; deaths=myDeaths; color=myColor; otherPlayers=newOthers}
        | UpdateTime(t, dt) ->
            let model = {model with time = t;lastDt=dt}
            let model = model |> cam (CameraMessage.UpdateTime(t, dt))
            let newTrailSet = 
                model.shotTrails
                |> HashSet.filter (fun trail -> trail.duration + trail.startTime > t)
            let model = { model with shotTrails = newTrailSet}
            if model.triggerHeld && model.activeWeapon=RainbowGun then env.Emit [Shoot]
            let updatedWeapons = 
                if Model.controlsDisabled model then model.weapons
                else
                    model.weapons
                    |> HashMap.map (fun weaponType weapon -> 
                        match weapon.ammo with
                        | Endless -> weapon
                        | Limited ammoInfo ->
                            match ammoInfo.startReloadTime with 
                            | Not -> weapon
                            | Reloading startTime -> 
                                if model.activeWeapon = weaponType then
                                    let isFinished = startTime + ammoInfo.reloadTime <= t
                                    if isFinished then
                                        {weapon with ammo = (weapon.reload weapon.ammo)}
                                    else
                                        weapon
                                else
                                    let remaining = t - startTime
                                    {weapon with ammo = Limited {ammoInfo with startReloadTime = PausedReload remaining}} 
                            | PausedReload rem -> 
                                if model.activeWeapon = weaponType then 
                                    {weapon with ammo = Limited {ammoInfo with startReloadTime = Reloading (model.time - rem)}} 
                                else weapon
                    )
            let model = { model with weapons = updatedWeapons}

            let model = 
                {model with 
                    explosionAnimations = 
                        model.explosionAnimations
                        |> HashSet.filter (fun i ->
                            i.StartTime+i.Duration > t
                        )
                }
            let model = {model with gotHitIndicatorInstances = model.gotHitIndicatorInstances |> HashSet.filter(fun i -> i.StartTime+PlayerConstant.gotHitMarkerDuration>model.time)}
            let model = {model with hitEnemyIndicatorInstances = model.hitEnemyIndicatorInstances |> HashMap.filter(fun _ st -> st+PlayerConstant.hitEnemyMarkerDuration>model.time)}
            let model = 
                match model.gameEndTime with 
                | None -> 
                    let myt = model.serverTime
                    if model.gameStartTime+PlayerConstant.roundTime < myt then 
                        {model with gameEndTime = Some t}
                    else model
                | Some _ -> model
            let model = 
                {model with 
                    gunAnimationState = 
                        { model.gunAnimationState with
                            lastFw = model.camera.camera.Forward
                        }
                }

            let model =
                { model with
                    hitAnimations = 
                        model.hitAnimations |> HashSet.filter (fun a ->
                            let endTime = a.startTime + a.duration
                            endTime >= t
                        )
                }
            env.Emit [UpdateProjectiles dt]
            client.send (
                let reolading = model.weapons |> HashMap.tryFind model.activeWeapon |> Option.map Weapon.isReloading |> Option.defaultValue false
                NetworkCommand.UpdatePosition(
                    (if (Model.amDead model) then V3d(234234234.0,346346346.0,35757457.0) else model.camera.camera.Location),
                    model.camera.camera.Forward,
                    model.activeWeapon |> WeaponType.pickle,
                    reolading
                )
            )
            if model.onFloor then
                model
            else
                let cam = model.camera
                { model with
                    camera = 
                        if (Model.controlsDisabled model) then cam
                        else 
                            { cam with 
                                velocity = cam.velocity - V3d(0.0, 0.0, 20.81) * dt
                                move = cam.move.XYO
                            }
                } 
        | TeleportToSpawnLocation -> 
            let loc = model.world.SpawnLocation()
            let newCameraView = model.camera.camera.WithLocation(loc)
            let modelCamera = {model.camera with camera = newCameraView; velocity = V3d.Zero}
            {model with camera = modelCamera}
        | ResetPlayerState -> 
            {model with
                weapons = model.weapons |> HashMap.map (fun _ w -> {w with ammo = AmmunitionType.reload w.ammo})
                camera = {model.camera with velocity=V3d.Zero; blastVelocity=V3d.Zero}
                frags = 0
                deaths = 0
            }
        | Respawn force -> 
            let respawn() = 
                env.Emit [TeleportToSpawnLocation; ResetPlayerState]
                {model with currentHp=100; deathTime=None}
            if force then respawn()
            else 
                match model.deathTime with 
                | Some t when t+PlayerConstant.respawnDelay < model.time ->
                    respawn()
                | _ -> 
                    model
        | RestartGame force -> 
            let restart() =
                printfn "restarting game"
                env.Emit [ResetWorldState;Respawn true]
                {model with gameEndTime=None}
            if force then restart()
            else
                match model.gameEndTime with 
                | None -> restart()
                | Some et -> 
                    if et+PlayerConstant.roundRestartDelay > model.time then model 
                    else restart()
        | ResetWorldState -> 
            {model with projectiles=HashSet.empty; shotTrails=HashSet.empty; explosionAnimations=HashSet.empty; gotHitIndicatorInstances=HashSet.empty; hitEnemyIndicatorInstances=HashMap.empty}
        | UpdateAnimationState s -> {model with gunAnimationState=s}
        | UpdateProjectiles dt -> 
            let newProjs = 
                let emitExplosion e =
                    client.send (NetworkCommand.Explode(e.Owner, e.Position, e.SmallRadius, e.BigRadius, e.SmallDamage, e.BigDamage))
                    env.Emit [Explode e]
                Projectile.calculateHits
                    model.projectiles
                    model.playerName
                    model.time
                    dt
                    model.world
                    model.otherPlayers
                    emitExplosion
            let newProjs =
                newProjs |> HashSet.map (fun pi -> 
                    let newTrail =
                        match pi.Trail |> List.tryHead with 
                        | Some h -> 
                            if Vec.distance pi.Position h >= 2.0 then 
                                pi.Position::(pi.Trail |> List.truncate 4)
                            else 
                                pi.Trail
                        | None -> [pi.Position]
                    {pi with Trail = newTrail}
                )
            {model with projectiles = newProjs}
        | Explode e -> 
            let myHit, otherHits = Projectile.explode e model.playerName model.camera.camera.Location model.otherPlayers
            match myHit with 
            | None -> ()
            | Some (vel,dmg) -> 
                env.Emit [HitByWithSlap(e.Owner, dmg, vel, e.Position, WeaponType.RocketLauncher)]
            otherHits |> HashMap.iter (fun name (vel,dmg) -> 
                env.Emit [HitEnemyWithSlap(name,dmg,vel,e.Position, WeaponType.RocketLauncher)]
            )
            let anim =
                {
                    Center = e.Position
                    SmallRadius = e.SmallRadius
                    BigRadius = e.BigRadius
                    StartTime = model.time
                    Duration = 1.0
                    BigColor = C4b.Gold
                    SmallColor = C4b.LightYellow
                }
            env.Emit [CreateExplosionAnimation anim]
            model
        | CreateExplosionAnimation anim -> 
            {model with explosionAnimations = model.explosionAnimations |> HashSet.add anim}
        | KeyDown _ -> model
        | KeyUp _ -> model
        | MouseScroll x -> 
            let weaponSwitch =
                match x < 0 with
                | true -> 
                   match  model.activeWeapon with
                   | LaserGun -> Shotgun
                   | Shotgun -> Sniper
                   | Sniper -> RainbowGun
                   | RainbowGun -> RocketLauncher
                   | RocketLauncher -> LaserGun
                | false -> 
                    match model.activeWeapon with
                    | LaserGun -> RocketLauncher
                    | Shotgun -> LaserGun
                    | Sniper -> Shotgun
                    | RainbowGun -> Sniper
                    | RocketLauncher -> RainbowGun
            {model with activeWeapon = weaponSwitch}

        | MouseUp button ->
            match button with
            |MouseButtons.Left -> {model with triggerHeld=false}
            |MouseButtons.Right -> {model with proj = Frustum.perspective 110.0 0.1 1000.0 (float model.size.X / float model.size.Y) ; isZoomed = false}
            | _ -> model
        | MouseDown button -> 
            let nm = 
                match button with
                | MouseButtons.Left -> 
                    if (Model.controlsDisabled model) then model
                    else
                        let messages = [Shoot]
                        env.Emit messages
                        {model with triggerHeld=true}
                | MouseButtons.Right -> 
                    let weapon = model.weapons.Item model.activeWeapon
                    match weapon.name with
                    | "Lasergun" -> {model with proj = Frustum.perspective 90.0 0.1 1000.0 (float model.size.X / float model.size.Y) ; isZoomed = true}
                    | "Shotgun" -> model
                    | "Sniper" ->
                        {model with proj = Frustum.perspective 30.0 0.1 1000.0 (float model.size.X / float model.size.Y) ; isZoomed = true}
                    | _ -> model
                | _ -> model
            nm 
        | SpawnProjectiles ps -> 
            let ps = ps |> List.map (fun pi -> {pi with StartTime=model.time})
            {model with projectiles = HashSet.union (HashSet.ofList ps) model.projectiles}
        | SpawnShotTrails trails -> 
            let nts = HashSet.union (HashSet.ofList (trails |> List.map (fun (line,dur,color) -> {realLine=line;offsetLine=line;duration=dur;startTime=model.time;color=color}))) model.shotTrails
            {model with shotTrails = nts}
        | Shoot -> 
            let weapon = model.weapons.Item model.activeWeapon
            let weapon = 
                match weapon.ammo with
                | Endless -> weapon
                | Limited ammoInfo -> 
                    match ammoInfo.availableShots <= 0 && (not (Model.controlsDisabled model)) with
                    | false -> weapon
                    | true -> {weapon with ammo =  weapon.startReload weapon.ammo model.time}
             
            let canShoot = 
                weapon.canShoot weapon.ammo weapon.lastShotTime weapon.waitTimeBetweenShots model.time
                && (not (Model.controlsDisabled model))
            match canShoot with
            | false -> {model with weapons = model.weapons |> HashMap.add model.activeWeapon weapon}
            | true -> 
                let shotRays = weapon.createHitrays model.camera.camera
                let projectiles = 
                    let infos = weapon.createProjectiles model.camera.camera
                    let projs = 
                        infos |> List.map (fun i -> 
                            {
                                Owner = model.playerName
                                Position = i.pos
                                Velocity = i.vel
                                StartTime = model.time
                                MaxDuration = 20.0
                                ExplosionSmallRadius = i.smallRadius
                                ExplosionBigRadius = i.bigRadius
                                ExplosionSmallDamage = i.smallDmg
                                ExplosionBigDamage = i.bigDmg
                                Trail = []
                            }
                        )
                    let networkProjs = 
                        projs |> List.map (fun i -> 
                            i.Owner, i.Position, i.Velocity, i.MaxDuration, i.ExplosionSmallRadius, i.ExplosionBigRadius, i.ExplosionSmallDamage, i.ExplosionBigDamage
                        )
                    client.send (NetworkCommand.SpawnProjectiles networkProjs)
                    projs
                    

                let hittedTargets,hitPlayers,floorHits = Weapon.findHitTargets weapon.range shotRays model.world model.targets model.otherPlayers model.camera.camera
                let playerHits = hitPlayers |> List.map (fun (i,_,p) -> i,p) |> HashMap.ofList
                let newTrails = 
                    let unclippedTrails = weapon.createShottrails weapon.range shotRays model.camera.camera model.time
                    let floorHits = floorHits |> HashMap.ofList
                    let targetHits = hittedTargets |> List.map (fun (i,_,p) -> i,p) |> HashMap.ofList
                    let newTrails = 
                        List.mapi (fun i (n : TrailInfo) -> 
                            match floorHits |> HashMap.tryFind i with 
                            | Some floor -> 
                                match playerHits |> HashMap.tryFind i, targetHits |> HashMap.tryFind i with
                                | Some hit, _ | None, Some hit -> {n with offsetLine=Line3d(n.offsetLine.P0, hit)}
                                | _ -> {n with offsetLine=Line3d(n.offsetLine.P0, floor)}
                            | _ -> n
                        ) unclippedTrails 
                    client.send (NetworkCommand.SpawnShotTrails (newTrails |> List.map (fun info -> info.realLine, 0.0, info.duration, info.color)))
                    newTrails
                let mutable newHits = 
                    floorHits |> List.choose (fun (i,p) ->
                        if playerHits |> HashMap.containsKey i then None
                        else Some {
                                position = p
                                color = C4b.Wheat
                                startTime = model.time
                                duration = 0.5
                            }
                    )
                    |> HashSet.ofList

                hitPlayers
                |> List.groupBy (fun (_,a,_) -> a) 
                |> List.iter (fun (otherPlayerName,count) -> 
                    let hitCount = count |> List.length
                    let dmg = weapon.calculateDamage hitCount
                    if dmg > 0 then
                        let (_,_, p) = count |> List.head
                        let newHit =
                            {
                                position = p
                                color = C4b.Red
                                startTime = model.time
                                duration = 1.0
                            }
                        newHits <- HashSet.add newHit newHits
                        env.Emit [HitEnemy(otherPlayerName, dmg, model.camera.camera.Location, model.activeWeapon)]
                )

                let updatedTargets = 
                    let damaged = weapon.processHits (List.map (fun (_,a,_) -> a) hittedTargets) model.targets
                    HashMap.union
                        model.targets
                        damaged
                    |> HashMap.filter (fun _ t -> t.currentHp > 0)
                let updatedWeapon = {weapon with ammo = weapon.updateAmmo weapon.ammo; lastShotTime = Some model.time }
                
                env.Emit [SpawnShotTrails (newTrails |> List.map (fun ti -> ti.offsetLine,ti.duration,ti.color)); SpawnProjectiles projectiles]
                { model with
                    targets = updatedTargets
                    weapons = model.weapons |> HashMap.add model.activeWeapon updatedWeapon
                    hitAnimations = HashSet.union model.hitAnimations newHits
                }
        | KillfeedMessage(k,d,w) -> 
            let s = WeaponType.toString w
            let s = sprintf "%s fragged %s with %s" k d s
            let nkf = (model.time,s)::(model.killfeed |> List.truncate killfeedLength)
            {model with killfeed=nkf}
        | EnemyDied (k,d,w) -> 
            let animMsg = 
                match model.otherPlayers |> HashMap.tryFind d with 
                | Some o when d<>model.playerName && k=model.playerName-> 
                    [
                        CreateExplosionAnimation {
                            Center = o.pos - V3d.OOI*0.5
                            SmallRadius = 0.25
                            BigRadius = 0.45
                            StartTime = model.time
                            Duration = 0.5
                            BigColor = C4b.MediumVioletRed
                            SmallColor = C4b.OrangeRed
                        }
                    ]
                | _ -> []
            env.Emit 
                [
                    yield KillfeedMessage(k,d,w)
                    yield! animMsg
                ]
            model
        | CreateGotHitIndicatorInstance(sd,dmg) -> 
            let newInst =
                {
                    HitPosition = sd
                    StartTime = model.time
                    damage = dmg
                }
            {model with gotHitIndicatorInstances = model.gotHitIndicatorInstances |> HashSet.add newInst}
        | CreateHitEnemyIndicatorInstance n -> 
            {model with hitEnemyIndicatorInstances = model.hitEnemyIndicatorInstances |> HashMap.add n model.time}
        | HitEnemyWithSlap(n,d,v,sp,w) -> 
            env.Emit [CreateHitEnemyIndicatorInstance n]
            model
        | HitEnemy(n,d,v,w) -> 
            client.send (NetworkCommand.Hit(n,d,v,WeaponType.pickle w))
            env.Emit [CreateHitEnemyIndicatorInstance n]
            model