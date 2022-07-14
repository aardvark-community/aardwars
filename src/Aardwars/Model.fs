namespace Elm

open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Application
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Base
open Aardwars
open Aardvark.Rendering.Text

open Aardwars.Gun

type World =
    {
        Hit : V3d -> V3d -> V3d * bool
        Scene : IRenderWindow -> ISg
    }

module World =
    let plane(z : float) =
        let hit (p0 : V3d) (p1 : V3d) =
            let mutable p1 = p1
            if p1.Z <= z then
                p1.Z <- z
                p1, true
            else
                p1, false
        let sg = 
            Sg.fullScreenQuad
            |> Sg.scale 100.0
            |> Sg.diffuseTexture DefaultTextures.checkerboard
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }
        {
            Hit = hit
            Scene = fun _ -> sg
        }

    let randomGenerated blockCount gridSize (playerHeight : float) =
        let worldBoxes, render = Scene.createTestScene blockCount gridSize
        let sg = Scene.boxSg render
        let hit (p0 : V3d) (p1 : V3d) =
            let p0 = p0 - (V3d.OOI * playerHeight)
            let mutable p1 = p1 - (V3d.OOI * playerHeight)

            let mutable zUp = false
            let tryRepair p1 =
                worldBoxes 
                |> Array.fold (fun p1 b ->
                    let pos = Motion.repairMotion p0 p1 b 0.2
                    match pos with
                    | Some (z, p) -> 
                        if z then zUp <- true
                        p
                    | None -> p1
                ) p1
            let p1 = tryRepair p1
            let newP1 = p1 + (V3d.OOI * playerHeight)
            let onFloor =                        
                if zUp then true
                else newP1.Z <= playerHeight
            newP1,onFloor
        {
            Hit = hit
            Scene = fun _ -> sg
        }
    
[<ModelType>]
type CameraModel =
    {
        look        : bool
        move        : V3d
        velocity    : V3d
        camera      : CameraView
    }



type Target =
    {
        currentHp : int
        maxHp : int
        pos : V3d
        radius : float
    }

module Target =
    let GetTargetColor (currentHp : int) (maxHp : int) =
        if currentHp = maxHp then C4b(0uy, 255uy, 0uy, 255uy)
        elif currentHp = 0 then C4b.Red
        else 
            let t = float currentHp / float maxHp
            let t = 1.0 - t
            let t = t * 255.0
            let r = byte t
            let g = 255uy - r
            C4b(r, g, 0uy, 255uy)
    let SceneGraph (t : Target) (name : string) = 
        let cfg : TextConfig =
            {
                align = TextAlignment.Center
                color = C4b.White
                flipViewDependent = true
                font = FontSquirrel.Hack.Regular
                renderStyle = RenderStyle.NoBoundary
            }    
                
        let textSg = 
            Sg.textWithConfig cfg (AVal.constant (sprintf "%s: %d" name t.currentHp))
            |> Sg.transform (
                Trafo3d.RotationXInDegrees(90.0) *
                Trafo3d.Translation(t.pos + V3d.OOI * (t.radius+0.5))  
            )


        let color = GetTargetColor t.currentHp t.maxHp
        let sphereSg = 
            Sg.sphere' 2 color t.radius
            |> Sg.translation' t.pos
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
            }
        Sg.ofList [textSg; sphereSg]
        

type LastHitInfo = 
    {
        name        : string
        hitSeries   : int
    }


[<ModelType>]
type Model =
    {
        [<NonAdaptive>]
        world       : World
        targets     : HashMap<string,Target>
        onFloor     : bool
        time        : float
        size        : V2i
        camera      : CameraModel
        proj        : Frustum
        moveSpeed   : float
        airAccel    : float
        lastHit     : Option<LastHitInfo>
        weapons     : HashMap<WeaponType,Weapon>
        activeWeapon: WeaponType

    }


