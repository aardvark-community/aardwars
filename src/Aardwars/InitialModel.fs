
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

    let intitial (env : Environment<Message>) = 
        
        //let world = World.randomGenerated 0 (V2i(150,150)) 1.75
        let world = 
            let textures = @"C:\minecraft\textures"
            let map = @"C:\minecraft\Small Worlds"
            let atlas, tree = MinecraftWorld.load env.Runtime textures map
            World.minecraft env.Window atlas tree 1.75

        let center = world.Bounds.Center.XYO + world.Bounds.Max.OOZ + V3d(0.1, 0.2, 0.4)
        
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
            time = 0.0
            lastDt = 0.1
            targets = initialTargets
            moveSpeed = 10.0
            airAccel = 0.0015
            weapons = HashMap.ofArray[|
                    Primary,Weapon.laserGun
                    Secondary,Weapon.shotGun
                |]
            activeWeapon = Primary
            shotTrails = HashSet.empty
            gunAnimationState = AnimationState.initial
            otherPlayers = HashMap.empty
            hp = 100.0
        }

    let view (client : NetworkClient) (env : Environment<Message>) (model : AdaptiveModel) =
        
        Update.events client env
        let worldSg = model.world.Scene env.Window

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


        let textSg = 
            let velocitySg = 
                Text.velocityTextSg 
                    env.Window 
                    (model.camera.velocity |> AVal.map (fun v -> sprintf "%.2f" v.Length)) 
                    (AVal.constant true)
            
            let statsSg = 

                let weapon = 
                    model.activeWeapon 
                    |> AVal.bind (fun weaponType -> 
                        model.weapons |> AMap.find weaponType
                    )

                let text = 
                    (weapon, model.hp) 
                    ||> AVal.map2 (fun w hp -> 
                        match w.ammo with
                        | Endless -> sprintf "Weapon: %s\tAmmo: Inf\tHP:%.0f" w.name hp
                        | Limited ammoInfo -> 
                            let rld = 
                                if ammoInfo.startReloadTime |> Option.isSome then " (reloading ...)"
                                else ""
                            sprintf "Weapon: %s\tAmmo: %i/%i%s\tHP:%.0f" w.name ammoInfo.availableShots ammoInfo.maxShots rld hp
                    )

                Text.weaponTextSg env.Window text

            [ velocitySg; statsSg ] |> Sg.ofList
                
        let trailsSg = Trails.sg model.shotTrails model.time |> Sg.pass Passes.pass1
            
        let targetsSg =
            model.targets 
            |> AMap.toASet 
            |> ASet.map (fun (name,t) -> 
                Target.SceneGraph t name
            ) |> Sg.set
            
        let otherPlayers =
            let trafos = 
                model.otherPlayers 
                |> AMap.toAVal
                |> AVal.map (HashMap.toValueArray >> Array.map (fun pi -> pi.pos |> Trafo3d.Translation))

            Sg.box' C4b.Yellow playerBounds
            |> Sg.instanced trafos
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.simpleLighting
            }


        Sg.ofList [worldSg; gunSg; textSg; targetsSg; trailsSg; otherPlayers; Skybox.scene]
            |> Sg.viewTrafo (model.camera.camera |> AVal.map CameraView.viewTrafo)
            |> Sg.projTrafo (model.proj |> AVal.map Frustum.projTrafo)

