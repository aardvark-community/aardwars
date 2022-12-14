namespace Elm

open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Application
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Base
open Aardwars
open Aardvark.Rendering.Text



[<ModelType>]
type CameraModel =
    {
        look        : bool
        move        : V3d
        velocity    : V3d
        blastVelocity : V3d
        camera      : CameraView
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
        

        
type AnimationState = 
    { 
        t : V3d 
        a : V3d
        lastFw : V3d
    }
module AnimationState = 
    let initial = 
        { 
            t = V3d.Zero 
            a = V3d.Zero
            lastFw = V3d.Zero
        } 

type HitAnimation =
    {
        position    : V3d
        startTime   : float
        duration    : float
        color       : C4b
    }
    
type ProjectileInfo =
    {
        Owner : string
        Position : V3d
        Velocity : V3d
        StartTime : float
        MaxDuration : float
        ExplosionSmallRadius : float
        ExplosionBigRadius : float
        ExplosionSmallDamage : float
        ExplosionBigDamage : float
        Trail : list<V3d>
    }
    
type ExplosionInfo =
    {
        Owner : string
        Position : V3d
        SmallRadius : float
        BigRadius : float
        SmallDamage : float
        BigDamage : float
    }
type ExplosionAnimationInfo =
    {
        Center : V3d
        SmallRadius : float
        BigRadius : float
        StartTime : float
        Duration : float
        SmallColor : C4b
        BigColor : C4b
    }
type GotHitIndicatorInstance =
    {
        HitPosition : V3d
        StartTime : float
        damage : float
    }

[<ModelType>]
type Model =
    {
        [<NonAdaptive>]
        world               : World
        targets             : HashMap<string,Target>
        onFloor             : bool
        time                : float
        lastDt              : float
        size                : V2i
        camera              : CameraModel
        proj                : Frustum
        isZoomed            : bool
        playerName          : string
        frags               : int
        deaths              : int
        color               : string
        moveSpeed           : float
        airAccel            : float
        weapons             : HashMap<WeaponType,Weapon>
        activeWeapon        : WeaponType
        shotTrails          : HashSet<TrailInfo>
        gunAnimationState   : AnimationState
        otherPlayers        : HashMap<string, OtherPlayerInfo>
        projectiles         : HashSet<ProjectileInfo>
        currentHp           : float
        maxHp               : float
        hitAnimations       : HashSet<HitAnimation>
        explosionAnimations : HashSet<ExplosionAnimationInfo>
        triggerHeld         : bool
        killfeed            : list<float*string>
        gotHitIndicatorInstances : HashSet<GotHitIndicatorInstance>
        hitEnemyIndicatorInstances : HashMap<string,float>
        lastPositionReset : float
        lastGotHit : Option<string * WeaponType>
        deathTime : Option<float>
        gameEndTime : Option<float>
        gameStartTime : float
        serverTime : float
        
        tabDown             : bool
        ctrlDown            : bool
        shiftDown           : bool
    }
module Model =
    let amDead (m : Model) =
        m.deathTime |> Option.isSome
    let gameIsRunning (m : Model) =
        m.gameEndTime |> Option.isNone
    let controlsDisabled (m : Model) = 
        amDead m || not (gameIsRunning m)


