
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

    let intitial (texturesPath : string) (mapPath : string) (env : Environment<Message>) = 
        
        //let world = World.randomGenerated 0 (V2i(150,150)) 1.75
        let world = 
            let atlas, tree = MinecraftWorld.load env.Runtime texturesPath mapPath
            World.minecraft env.Window atlas tree 1.75
        let random = System.Random()

        let center = world.Bounds.Min.XYO + world.Bounds.RangeZ.Center * V3d.OOI + V3d.IIO + V3d(random.Next(45,100),random.Next(45,100),random.Next(-40,-35))
        
        let cam = { CameraController.initial with camera = CameraView.lookAt center (center + V3d.IOO) V3d.OOI }

        let (p1, floor) = 
            world.Hit (cam.camera.Location + V3d(0,0,1000)) cam.camera.Location

        let random = System.Random() 
            
        let initialTargets = 
            HashMap.ofList [
                for i = 0 to 20 - 1 do
                    let randomHealth = random.Next(10,200)
                    let randomRadius = random.Next(3,10)
                    let randomPosition = V3d(random.Next(-50,50),random.Next(-50,50),random.Next(3,20))
                    let name = sprintf "target_%i" i
                    name,{currentHp = randomHealth; maxHp = randomHealth; pos = randomPosition ;radius = randomRadius }   
            ]
        
        {
            world = world
            onFloor = floor
            size = V2i.II
            camera = { cam with camera = cam.camera.WithLocation(p1) }
            proj = Frustum.perspective 150.0 0.1 1000.0 1.0
            isZoomed = false
            time = 0.0
            lastDt = 0.1
            targets = initialTargets
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
                //["sadasd",
                //                                {
                //                                    color = "blue"
                //                                    pos = center
                //                                    frags = 0
                //                                    deaths = 0
                //                                }]
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
            tabDown=false
        }
        
    let playerModels = 
        HashMap.ofList [
            let s = "black" in yield s, Import.importPlayer "player" s
            let s = "blue" in yield s, Import.importPlayer "player" s
            let s = "green" in yield s, Import.importPlayer "player" s
            let s = "orange" in yield s, Import.importPlayer "player" s
            let s = "pink" in yield s, Import.importPlayer "player" s
            let s = "purple" in yield s, Import.importPlayer "player" s
            let s = "red" in yield s, Import.importPlayer "player" s
            let s = "white" in yield s, Import.importPlayer "player" s
            let s = "yellow" in yield s, Import.importPlayer "player" s
        ]
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
                    ||> AVal.map2 (fun w hp -> 
                        match w.ammo with
                        | Endless -> sprintf "HP:%.0f\tAmmo: Inf" hp
                        | Limited ammoInfo -> 
                            let rld = 
                                if ammoInfo.startReloadTime |> Option.isSome then " (reloading ...)"
                                else ""
                            sprintf "HP:%.0f\tAmmo: %i/%i%s" hp ammoInfo.availableShots ammoInfo.maxShots rld 
                    )

                Text.weaponTextSg env.Window text

            [ velocitySg; statsSg; scoreboardSg] |> Sg.ofList
                
        let trailsSg = Trails.sg model.shotTrails model.time |> Sg.pass Passes.pass1
        let projectileSg = Projectile.scene model.projectiles
        let explosionSg = Projectile.explosionScene model.time model.explosionAnimations
        let killfeedSg = Text.killfeed env.Window model.time model.killfeed
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
                    Trafo3d.Scale(1.0/5.0) *
                    Trafo3d.FromBasis(V3d.IOO,V3d.OOI,V3d.OIO,V3d.OOO) *
                    Trafo3d.Translation(0.0,0.0,-1.7) *
                    Trafo3d.Translation(info.pos)
                )
            let models = 
                model.otherPlayers
                |> AMap.map (fun name info -> playerModels.[info.color])
            models |> AMap.toASet |> ASet.map (fun (name,model) -> 
                model |> Sg.trafo (trafos |> AMap.find name)
            )
            |> Sg.set
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }

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
                do! DefaultSurfaces.simpleLighting
            }


        Sg.ofList 
            [
                worldSg
                gunSg
                textSg
                targetsSg
                trailsSg
                otherPlayers
                hitBoxes
                hits
                projectileSg
                explosionSg
                killfeedSg
                Skybox.scene
            ]
            |> Sg.viewTrafo (model.camera.camera |> AVal.map CameraView.viewTrafo)
            |> Sg.projTrafo (model.proj |> AVal.map Frustum.projTrafo)


