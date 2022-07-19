﻿namespace Aardwars

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
    | KeyDown of key : Keys
    | KeyUp of key : Keys
    | Resize of newSize : V2i
    | UpdateTime of seconds : float * delta : float
    | UpdateAnimationState of AnimationState
    | Shoot
    | UpdatePlayerPos of string * V3d
    | HitBy of string * float

module Update =
    let rand = RandomSystem()

    
    let events (client : NetworkClient) (env : Environment<Message>) =
        let win = env.Window
        let glfw = win.GetType().GetField("glfw", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> Silk.NET.GLFW.Glfw
        let hwin = win.GetType().GetField("win", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(win) :?> nativeptr<Silk.NET.GLFW.WindowHandle>
        let mutable mouseDelta = V2d.Zero
        win.Cursor <- Cursor.None
    
        client.receive.Add (fun msg ->
            match msg with
            | NetworkMessage.UpdatePosition(player, pos) ->
                env.Emit [UpdatePlayerPos(player, pos)]
            | NetworkMessage.Hit(player, dmg) ->
                env.Emit [HitBy(player, dmg)]
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
        
        let sw = System.Diagnostics.Stopwatch.StartNew()
        let mutable last = sw.Elapsed.TotalSeconds
        let timer = 
            env.StartTimer(4, fun () ->
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
                                let vn = Fun.Lerp(0.05, newModel.camera.velocity, newModel.camera.move)
                                V3d(vn.X,vn.Y,newModel.camera.move.Z)
                        } 
                }
            else
                { newModel with 
                    camera = 
                        { newModel.camera with 
                            velocity =      
                                //let vn = Fun.Lerp(0.05, newModel.camera.velocity, newModel.camera.move)
                                
                                let x = model.airAccel * newModel.camera.move.X + newModel.camera.velocity.X
                                let y = model.airAccel * newModel.camera.move.Y + newModel.camera.velocity.Y
                                V3d(x,y,newModel.camera.velocity.Z)
                        } 
                }

        match message with
        | HitBy(player, dmg) ->
            let hp = model.hp - dmg
            if hp <= 0.0 then
                client.send (NetworkCommand.Died player)

                let b = model.world.Bounds
                let respawnLocation = 
                    let p = rand.UniformV2d(Box2d(b.Min.XY, b.Max.XY))
                    V3d(p, b.Max.Z)
                let newCameraView = model.camera.camera.WithLocation(respawnLocation)
                let modelCamera = { model.camera with camera = newCameraView  }

                { model with
                    hp = 100.0
                    camera = modelCamera
                }
            else
                { model with hp = hp }


        | UpdatePlayerPos(player, pos) ->
            { model with otherPlayers = HashMap.add player {pos=pos} model.otherPlayers }

        | MouseMove delta -> model |> cam (CameraMessage.Look delta)
        | KeyDown Keys.W -> model |> cam (CameraMessage.StartMove (V3d(0.0, model.moveSpeed, 0.0)))
        | KeyUp Keys.W -> model |> cam (CameraMessage.StopMove (V3d(0.0, model.moveSpeed, 0.0)))
        | KeyDown Keys.S -> model |> cam (CameraMessage.StartMove (V3d(0.0, -model.moveSpeed, 0.0)))
        | KeyUp Keys.S -> model |> cam (CameraMessage.StopMove (V3d(0.0, -model.moveSpeed, 0.0)))
        | KeyDown Keys.A  -> model |> cam (CameraMessage.StartMove (V3d(-model.moveSpeed, 0.0, 0.0)))
        | KeyUp Keys.A -> model |> cam (CameraMessage.StopMove (V3d(-model.moveSpeed, 0.0, 0.0)))
        | KeyDown Keys.D -> model |> cam (CameraMessage.StartMove (V3d(model.moveSpeed, 0.0, 0.0)))
        | KeyUp Keys.D -> model |> cam (CameraMessage.StopMove (V3d(model.moveSpeed, 0.0, 0.0)))
        | KeyDown Keys.Space -> model |> cam (CameraMessage.StartMove (V3d(0.0, 0.0, 10.0)))
        | KeyUp Keys.Space -> model
        | KeyDown Keys.D1 -> { model with activeWeapon = Primary}
        | KeyDown Keys.D2 -> { model with activeWeapon = Secondary}
        | KeyDown Keys.R ->
            let weapon = model.weapons.Item model.activeWeapon
            let updatedWeapons = 
                model.weapons 
                |> HashMap.add model.activeWeapon {weapon with ammo = (weapon.startReload weapon.ammo model.time)}
            {model with weapons = updatedWeapons}
        | KeyDown Keys.Back -> 
            let respawnLocation = model.world.Bounds.Center.XYZ + V3d.OOI*10.0
            let newCameraView = model.camera.camera.WithLocation(respawnLocation)
            let modelCamera = { model.camera with camera = newCameraView  }
            { model with camera = modelCamera }
        | KeyDown Keys.O -> 
            let n = model.moveSpeed + 0.1
            printfn "moveSpeed %.2f" n
            { model with moveSpeed = n }
        | KeyDown Keys.P -> 
            let n = model.moveSpeed - 0.1
            printfn "moveSpeed %.2f" n
            { model with moveSpeed = n }
        | KeyDown Keys.U -> 
            let n = model.airAccel + 0.00005
            printfn "airAccel %f" n
            { model with airAccel = n }
        | KeyDown Keys.I -> 
            let n = model.airAccel - 0.00005
            printfn "airAccel %f" n
            { model with airAccel = n }
        | Resize s -> 
            { model with 
                size = s
                proj = Frustum.perspective 110.0 0.1 1000.0 (float s.X / float s.Y) 
            }
        | UpdateTime(t, dt) ->
            let model = model |> cam (CameraMessage.UpdateTime(t, dt))
            let newTrailSet = 
                model.shotTrails
                |> HashSet.filter (fun trail -> trail.duration + trail.startTime > t)
            let model = { model with shotTrails = newTrailSet}

            let updatedWeapons = 
                model.weapons
                |> HashMap.map (fun weaponType weapon -> 
                    match weapon.ammo with
                    | Endless -> weapon
                    | Limited ammoInfo ->
                        match ammoInfo.startReloadTime with 
                        | None -> weapon
                        | Some startTime -> 
                            if model.activeWeapon = weaponType then
                                let isFinished = startTime + ammoInfo.reloadTime <= t
                                if isFinished then
                                    {weapon with ammo = (weapon.reload weapon.ammo)}
                                else
                                    weapon
                            else
                                {weapon with ammo = Limited { ammoInfo with startReloadTime = None } } 
                )
            let model = { model with weapons = updatedWeapons}


            let model = 
                {model with 
                    gunAnimationState = 
                    {model.gunAnimationState with
                        lastFw = model.camera.camera.Forward
                    }
                }

            client.send (NetworkCommand.UpdatePosition model.camera.camera.Location)

            if model.onFloor then
                { model with
                    time = t
                    lastDt = dt
                }
            else
                let cam = model.camera
                { model with
                    time = t
                    lastDt = dt
                    camera = 
                        { cam with 
                            velocity = cam.velocity - V3d(0.0, 0.0, 20.81) * dt
                            move = cam.move.XYO
                        }
                }
        | UpdateAnimationState s -> {model with gunAnimationState=s}
        | KeyDown _ 
        | KeyUp _ 
        | MouseUp _ -> 
            model
        | MouseDown button -> 
            match button with
                | MouseButtons.Left -> 
                    let messages = [Shoot]
                    env.Emit messages
                    model
                | MouseButtons.Right -> model
                | _ -> model
            
        | Shoot -> 
            let weapon = model.weapons.Item model.activeWeapon
            let weapon = 
                match weapon.ammo with
                | Endless -> weapon
                | Limited ammoInfo -> 
                    match ammoInfo.availableShots <= 0 with
                    | false -> weapon
                    | true -> {weapon with ammo =  weapon.startReload weapon.ammo model.time}
            let canShoot = weapon.canShoot weapon.ammo
            match canShoot with
            | false -> {model with weapons = model.weapons |> HashMap.add model.activeWeapon weapon}
            | true -> 
                let shotRays = weapon.createHitrays model.camera.camera

                let shotTrail = 
                    let newTrails = weapon.createShottrails weapon.range shotRays model.camera.camera model.time
                    HashSet.union (HashSet.ofList newTrails) model.shotTrails
                let hittedTargets,hitPlayers = weapon.findHitTargets weapon.range shotRays model.targets model.otherPlayers model.camera.camera

                hitPlayers
                |> List.groupBy id 
                |> List.iter (fun (otherPlayerName,count) -> 
                    let hitCount = count |> List.length
                    let dmg = weapon.calculateDamage hitCount
                    if dmg > 0 then
                        client.send (NetworkCommand.Hit(otherPlayerName, dmg))
                )

                let updatedTargets = 
                    let damaged = weapon.processHits hittedTargets model.targets
                    HashMap.union
                        model.targets
                        damaged
                    |> HashMap.filter (fun _ t -> t.currentHp > 0)
                let updatedWeapon = {weapon with ammo = weapon.updateAmmo weapon.ammo}

                { model with
                    targets = updatedTargets
                    weapons = model.weapons |> HashMap.add model.activeWeapon updatedWeapon
                    shotTrails = shotTrail
                }
