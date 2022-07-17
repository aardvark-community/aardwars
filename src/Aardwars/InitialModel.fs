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

open Aardwars.Gun

module Game =

    let intitial (env : Environment<Message>) = 
        
        let world = World.randomGenerated 5000 (V2i(150,150)) 1.75
        //let world = 
        //    let textures = @"C:\temp\aardwars\assets\textures"
        //    let map = @"C:\temp\aardwars\Notre Dame and Medieval City"
        //    let atlas, tree = MinecraftWorld.load env.Runtime textures map
        //    World.minecraft env.Window atlas tree 1.75

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
            proj = Frustum.perspective 90.0 0.1 1000.0 1.0
            time = 0.0
            targets = initialTargets
            moveSpeed = 10.0
            airAccel = 0.0001
            lastHit = None
            weapons = HashMap.ofArray[|
                    Primary,Weapon.laserGun
                    Secondary,Weapon.shotGun
                |]
            activeWeapon = Primary
            shotTrails = HashSet.empty
        }

    let view (env : Environment<Message>) (model : AdaptiveModel) =
        Update.events env
        let worldSg = model.world.Scene env.Window

        let gunSg = Weapon.scene env.Window model.activeWeapon
        let textSg = 
            Text.statusTextSg 
                env.Window 
                (model.camera.velocity |> AVal.map (fun v -> sprintf "%.2f" v.Length)) 
                (AVal.constant true)

        let trailsSg = Trails.sg model.shotTrails model.time
            
        let targetsSg =
            model.targets 
            |> AMap.toASet 
            |> ASet.map (fun (name,t) -> 
                Target.SceneGraph t name
            ) |> Sg.set
            
        Sg.ofList [worldSg; gunSg; textSg; targetsSg; trailsSg; Skybox.scene]
            |> Sg.viewTrafo (model.camera.camera |> AVal.map CameraView.viewTrafo)
            |> Sg.projTrafo (model.proj |> AVal.map Frustum.projTrafo)

