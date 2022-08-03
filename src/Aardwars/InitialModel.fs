
namespace Elm

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

module Game =

    let intitial (client : NetworkClient) (texturesPath : string) (mapPath : string) (env : Environment<Message>) = 
        
        let world = 
            let atlas, tree = MinecraftWorld.load env.Runtime texturesPath mapPath
            World.minecraft env.Window atlas tree 1.75
        
        let spawn = world.SpawnLocation() + V3d.OOI*50.0
        let cam = {CameraController.initial with camera = CameraView.lookAt spawn (spawn+V3d.IOO) V3d.OOI}

        let model = {
            world = world
            onFloor = false
            size = V2i.II
            camera = cam
            proj = Frustum.perspective 150.0 0.1 1000.0 1.0
            isZoomed = false
            time = 0.0
            lastDt = 0.1
            targets = HashMap.empty
            moveSpeed = 10.0
            airAccel = 0.0015
            weapons = HashMap.ofArray[|
                    LaserGun,Weapon.laserGun
                    Shotgun,Weapon.shotGun
                    Sniper,Weapon.sniper
                    RainbowGun,Weapon.rainbowgun
                    RocketLauncher,Weapon.rocketLauncher
                |]
            activeWeapon = LaserGun
            shotTrails = HashSet.empty
            gunAnimationState = AnimationState.initial
            otherPlayers = HashMap.empty
            currentHp = 100
            maxHp = 100
            hitAnimations = HashSet.empty
            explosionAnimations = HashSet.empty
            projectiles = HashSet.empty
            playerName = System.Environment.MachineName
            frags = 0
            deaths = 0
            color = "yellow"
            triggerHeld=false
            killfeed=[]
            gotHitIndicatorInstances=HashSet.empty
            hitEnemyIndicatorInstances=HashMap.empty
            deathTime=None
            lastPositionReset=0.0
            gameEndTime = None
            gameStartTime = 0.0
            lastGotHit=None
            serverTime = 0.0

            tabDown=false
            ctrlDown = false
            shiftDown = false
        }
        Update.update client env model (Respawn true)
        
    let playerModel = Import.importPlayer
    let playerTextures = 
        let inline impy s =
            s, Import.loadTexture (sprintf @"assets/player_%s.png" s)
        [
            "black"  
            "blue"   
            "green"  
            "orange" 
            "pink"   
            "purple" 
            "red"    
            "white"  
            "yellow" 
        ] 
        |> List.map impy
        |> HashMap.ofList
    let blackTexture        =  playerTextures.["black"]
    let blueTexture         =  playerTextures.["blue"]
    let greenTexture        =  playerTextures.["green"]
    let orangeTexture       =  playerTextures.["orange"]
    let pinkTexture         =  playerTextures.["pink"]
    let purpleTexture       =  playerTextures.["purple"]
    let redTexture          =  playerTextures.["red"]
    let whiteTexture        =  playerTextures.["white"]
    let yellowTexture       =  playerTextures.["yellow"]
    let view (client : NetworkClient) (env : Environment<Message>) (model : AdaptiveModel) =
        
        Update.events client env
        let worldSg = model.world.Scene env.Window

        let hits =
            let rand = RandomSystem()
            let cnt = 50
            let rps = 10.0
            let r = 0.0025
            let vel = 
                let w = Constant.PiTimesTwo * rps
                w*r
            let trafos = 
                model.hitAnimations |> ASet.mapA (fun hit ->

                    let arr = 
                        Array.init cnt (fun _ ->
                            let w = rand.UniformV3dDirection() *  rand.UniformDouble()
                            let v = rand.UniformV3dDirection() * 20.5 * rand.UniformDouble()
                            let rot = rand.UniformV3dDirection() * rand.UniformDouble() * Constant.PiTimesTwo |> Rot3d.FromAngleAxis
                            

                            (hit.position, rot, v, w)
                        )

                    let res = 
                        let sw = System.Diagnostics.Stopwatch.StartNew()
                        env.Window.Time |> AVal.stepTime (fun _ _ thing ->
                            let dt = sw.Elapsed.TotalSeconds
                            sw.Restart()
                            thing |> Array.map (fun (p : V3d, r : Rot3d, v : V3d, w : V3d) ->
                                let np = p + v*dt
                                let nr = r

                                let a = -v*v.Length * 6.0 - (p-hit.position) * 19.81 
                                let nv = v + a*dt
                                (np, nr, nv, w)
                            )
                        ) 
                        |> AVal.constant 
                        |> List.singleton 
                        |> AVal.integrate arr env.Window.Time
                    
                    res |> AVal.map (Array.map (fun (p,r,v,_) ->
                        let c = hit.color
                        (Trafo3d r * Trafo3d.Translation p), c
                    ))

                ) |> ASet.toAVal |> AVal.map Array.concat

            let things = 
                let res = trafos |> AVal.map Array.unzip
                Map.ofList [
                    "ModelTrafo", (typeof<Trafo3d>,res |> AVal.map (fst >> unbox))
                    "Color", (typeof<C4b>,res |> AVal.map (snd >> unbox))
                ]
            Sg.box' C4b.White (Box3d.FromCenterAndSize(V3d.Zero, V3d.III * 0.025))
            |> Sg.instanced' things
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.sgColor
                //do! DefaultSurfaces.simpleLighting
            }

        let gunSg = 
            let isReloading = 
                model.activeWeapon |> AVal.bind (fun a -> model.weapons |> AMap.tryFind a |> AVal.map (fun w -> w |> Option.map (fun (w:Weapon) -> Weapon.isReloading w || AmmunitionType.isEmpty w.ammo) |> Option.defaultValue false))
            Weapon.scene 
                (fun t a -> env.Emit [UpdateAnimationState {model.gunAnimationState.GetValue() with t = t; a = a}]) 
                env.Window 
                model.activeWeapon 
                model.camera.move 
                (model.camera.camera |> AVal.map (fun cv -> cv.Forward))
                model.lastDt
                (model.gunAnimationState |> AVal.map (fun s -> s.t))
                (model.gunAnimationState |> AVal.map (fun s -> s.a))
                (model.gunAnimationState |> AVal.map (fun s -> s.lastFw))
                isReloading
        

        //let medipackSg =
        //    PowerUps.scene model.world


        let textSg = 
            let velocitySg = 
                Text.velocityTextSg 
                    env.Window 
                    (model.camera.velocity |> AVal.map (fun v -> sprintf "%.2f" v.Length)) 
                    (AVal.constant true)
            let scoreboardSg = 
                Text.scoreboard 
                    env.Window 
                    model.tabDown 
                    model.frags 
                    model.deaths 
                    model.color 
                    model.playerName 
                    model.otherPlayers
            let statsSg = 

                let weapon = 
                    model.activeWeapon 
                    |> AVal.bind (fun weaponType -> 
                        model.weapons |> AMap.find weaponType
                    )

                let text = 
                    (weapon, model.currentHp) 
                    ||> AVal.bind2 (fun w hp -> 
                        match w.ammo with
                        | Endless -> AVal.constant (sprintf "HP: %.0f\nAmmo: Inf" hp)
                        | Limited ammoInfo -> 
                            match ammoInfo.startReloadTime with 
                            | Not ->  
                                AVal.constant (sprintf "HP:%.0f\nAmmo: %i/%i" hp ammoInfo.availableShots ammoInfo.maxShots)
                            | PausedReload st -> AVal.constant ""
                            | Reloading st -> 
                                model.time |> AVal.map (fun t -> 
                                    let left = ammoInfo.reloadTime - (t-st)
                                    (sprintf "HP:%.0f\nAmmo: %i/%i (reloading %.1f)" hp ammoInfo.availableShots ammoInfo.maxShots left)
                                )
                    )

                let onOff = (model.deathTime,model.gameEndTime) ||> AVal.map2 (fun d e -> (d |> Option.isNone) && (e |> Option.isNone))
                Text.weaponTextSg env.Window text
                |> Sg.onOff onOff

            [ velocitySg; statsSg; scoreboardSg] |> Sg.ofList
                
        let fw = model.camera.camera |> AVal.map (fun cv -> cv.Forward)
        let loc = model.camera.camera |> AVal.map (fun cv -> cv.Location)
        let trailsSg = Trails.sg model.shotTrails model.time |> Sg.pass Passes.pass1
        let projectileSg = Projectile.scene model.projectiles
        let explosionSg = Projectile.explosionScene model.time model.explosionAnimations
        let killfeedSg = Text.killfeed env.Window model.time model.killfeed
        let deathScreenSg = 
            let timeUntil = 
                (model.time,model.deathTime) ||> AVal.map2 (fun t dt -> dt|>Option.bind (fun dt -> 
                    let rem = dt+PlayerConstant.respawnDelay-t
                    if rem > 0.0 then Some rem else None
                ))
            Text.deathScreenSg env.Window (model.deathTime |> AVal.map Option.isSome) (model.lastGotHit |> AVal.map (Option.defaultValue ("",WeaponType.LaserGun))) timeUntil
        let gameOverScreenSg = 
            let timeUntil = 
                (model.time,model.gameEndTime) ||> AVal.map2 (fun t dt -> dt|>Option.bind (fun dt -> 
                    let rem = dt+PlayerConstant.roundRestartDelay-t
                    if rem > 0.0 then Some rem else None
                ))
            Text.gameOverScreenSg 
                env.Window 
                (model.gameEndTime |> AVal.map Option.isSome) 
                timeUntil 
                model.frags 
                model.deaths 
                model.color 
                model.playerName 
                model.otherPlayers
        let timeleftSg =
            let text = 
                (model.time,model.gameStartTime,model.serverTime) |||> AVal.map3 (fun _ st serverTime -> 
                    let rem = st+PlayerConstant.roundTime - serverTime
                    let min = sprintf "%d" (rem/60.0 |> int)
                    let sec = 
                        let v = rem%60.0 |> int
                        if v < 10 then sprintf "0%d" v else sprintf "%d" v
                    sprintf "%s:%s" min sec
                )
            let onoff = model.gameEndTime |> AVal.map Option.isNone
            Text.timeleftTextSg env.Window text onoff
        let gotHitIndicatorsSg = GotHitIndicatorInstance.scene model.time fw loc model.gotHitIndicatorInstances
        let hitEnemyIndicatorSg = HitEnemyMarkerInstance.scene model.time model.hitEnemyIndicatorInstances
        let targetsSg =
            model.targets 
            |> AMap.toASet 
            |> ASet.map (fun (name,t) -> 
                Target.SceneGraph t name
            ) |> Sg.set
            
        let otherPlayers =
            let trafos = 
                model.otherPlayers 
                |> AMap.map (fun name info -> 
                    let xyangle = 
                        let a = info.fw.XYO
                        let b = V3d.IOO
                        (atan2 (a.X*b.Y - a.Y*b.X) (a.X*b.X + a.Y*b.Y))
                    
                    Trafo3d.Scale(1.0/13.25) *
                    Trafo3d.FromBasis(V3d.IOO,V3d.OOI,V3d.OIO,V3d.OOO) *
                    Trafo3d.RotationZ -xyangle *
                    Trafo3d.Translation(0.0,0.0,-1.7) *
                    Trafo3d.Translation(info.pos)
                )

            let whiteness = 
                model.hitEnemyIndicatorInstances
                |> AMap.map (fun _ st -> 
                    model.time |> AVal.map (fun t -> t-st < PlayerConstant.hitEnemyMarkerDuration)
                )
            let models = 
                model.otherPlayers
                |> AMap.map (fun name info -> playerModel, info.color)
            let ii = AVal.constant Trafo3d.Identity
            //let gunTrafos = 
            //    model.otherPlayers |> AMap.map (fun _ i -> 
            //        let info = i
            //        Trafo3d.Scale(1.0/2.5) *
            //        Trafo3d.FromBasis(i.fw,i.ri,i.up,i.pos)
            //    )
            //let gunTypes = 
            //    model.otherPlayers |> AMap.map (fun name info -> info.weapon)
            //let gunReloadings = 
            //    model.otherPlayers |> AMap.map (fun name info -> info.reloading)
            //let guns = 
            //    model.otherPlayers |> AMap.map (fun name info -> 
            //        let trafo = gunTrafos |> AMap.find name
            //        let gun = gunTypes |> AMap.find name
            //        let rld = gunReloadings |> AMap.find name
            //        Weapon.gunModel gun rld ii
            //        |> Sg.trafo trafo
            //    )
            //    |> AMap.toASetValues
            //    |> Sg.set

            let players = 
                models |> AMap.toASet |> ASet.map (fun (name,(model,color)) -> 
                    let ci = 
                        match color with 
                        | "black" ->        0.0f
                        | "blue" ->         1.0f
                        | "green" ->        2.0f
                        | "orange" ->       3.0f
                        | "pink" ->         4.0f
                        | "purple" ->       5.0f
                        | "red" ->          6.0f
                        | "white" ->        7.0f
                        | "yellow" ->       8.0f
                        | _ -> 6969.0f
                    let isWhite = 
                        (whiteness |> AMap.tryFind name |> AVal.bind (fun o -> o |> Option.defaultValue (AVal.constant false)))
                        |> AVal.map (fun b -> 
                            if b then 1.0f else 0.0f
                        )
                    model 
                    |> Sg.adapter
                    |> Sg.trafo (trafos |> AMap.find name)
                    |> Sg.uniform "VollgasWhite" isWhite
                    |> Sg.uniform' "PlayerTex" ci
                )
                |> Sg.set
                |> Sg.shader {
                    do! DefaultSurfaces.trafo
                    do! Shader.playertexy
                    do! DefaultSurfaces.simpleLighting
                    do! Shader.vollgasWhite
                }
            Sg.ofList[players]
            |> Sg.texture' "blackPlayerTex" blackTexture 
            |> Sg.texture' "bluePlayerTex" blueTexture  
            |> Sg.texture' "greenPlayerTex" greenTexture 
            |> Sg.texture' "orangePlayerTex" orangeTexture
            |> Sg.texture' "pinkPlayerTex" pinkTexture  
            |> Sg.texture' "purplePlayerTex" purpleTexture
            |> Sg.texture' "redPlayerTex" redTexture   
            |> Sg.texture' "whitePlayerTex" whiteTexture 
            |> Sg.texture' "yellowPlayerTex" yellowTexture


        let hitBoxes =
            let trafos = 
                model.otherPlayers 
                |> AMap.toAVal
                |> AVal.map (HashMap.toValueArray >> Array.map (fun pi -> pi.pos |> Trafo3d.Translation))

            Sg.box' C4b.Yellow playerBounds
            |> Sg.instanced trafos
            |> Sg.fillMode' FillMode.Line            
            |> Sg.shader {
                do! DefaultSurfaces.trafo
            }


        Sg.ofList 
            [
                worldSg
                gunSg
                textSg
                targetsSg
                trailsSg
                otherPlayers
                deathScreenSg
                gameOverScreenSg
                timeleftSg
                //hitBoxes
                hits
                projectileSg
                explosionSg
                killfeedSg
                gotHitIndicatorsSg
                hitEnemyIndicatorSg
                Skybox.scene
            ]
            |> Sg.viewTrafo (model.camera.camera |> AVal.map CameraView.viewTrafo)
            |> Sg.projTrafo (model.proj |> AVal.map Frustum.projTrafo)


