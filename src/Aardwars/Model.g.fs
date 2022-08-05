//b40fe333-f952-ee92-2d9b-5f808b416b08
//cba0f649-7348-148d-56df-b0ec455de49c
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
#nowarn "1182" // value is unused
namespace rec Elm

open System
open FSharp.Data.Adaptive
open Adaptify
open Elm
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveCameraModel(value : CameraModel) =
    let _look_ = FSharp.Data.Adaptive.cval(value.look)
    let _move_ = FSharp.Data.Adaptive.cval(value.move)
    let _velocity_ = FSharp.Data.Adaptive.cval(value.velocity)
    let _blastVelocity_ = FSharp.Data.Adaptive.cval(value.blastVelocity)
    let _camera_ = FSharp.Data.Adaptive.cval(value.camera)
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : CameraModel) = AdaptiveCameraModel(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : CameraModel) -> AdaptiveCameraModel(value)) (fun (adaptive : AdaptiveCameraModel) (value : CameraModel) -> adaptive.Update(value))
    member __.Update(value : CameraModel) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<CameraModel>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _look_.Value <- value.look
            _move_.Value <- value.move
            _velocity_.Value <- value.velocity
            _blastVelocity_.Value <- value.blastVelocity
            _camera_.Value <- value.camera
    member __.Current = __adaptive
    member __.look = _look_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>
    member __.move = _move_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V3d>
    member __.velocity = _velocity_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V3d>
    member __.blastVelocity = _blastVelocity_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V3d>
    member __.camera = _camera_ :> FSharp.Data.Adaptive.aval<Aardvark.Rendering.CameraView>
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveModel(value : Model) =
    let _targets_ = FSharp.Data.Adaptive.cmap(value.targets)
    let _onFloor_ = FSharp.Data.Adaptive.cval(value.onFloor)
    let _time_ = FSharp.Data.Adaptive.cval(value.time)
    let _lastDt_ = FSharp.Data.Adaptive.cval(value.lastDt)
    let _size_ = FSharp.Data.Adaptive.cval(value.size)
    let _camera_ = AdaptiveCameraModel(value.camera)
    let _proj_ = FSharp.Data.Adaptive.cval(value.proj)
    let _isZoomed_ = FSharp.Data.Adaptive.cval(value.isZoomed)
    let _playerName_ = FSharp.Data.Adaptive.cval(value.playerName)
    let _frags_ = FSharp.Data.Adaptive.cval(value.frags)
    let _deaths_ = FSharp.Data.Adaptive.cval(value.deaths)
    let _color_ = FSharp.Data.Adaptive.cval(value.color)
    let _moveSpeed_ = FSharp.Data.Adaptive.cval(value.moveSpeed)
    let _airAccel_ = FSharp.Data.Adaptive.cval(value.airAccel)
    let _weapons_ = FSharp.Data.Adaptive.cmap(value.weapons)
    let _activeWeapon_ = FSharp.Data.Adaptive.cval(value.activeWeapon)
    let _shotTrails_ = FSharp.Data.Adaptive.cset(value.shotTrails)
    let _gunAnimationState_ = FSharp.Data.Adaptive.cval(value.gunAnimationState)
    let _otherPlayers_ = FSharp.Data.Adaptive.cmap(value.otherPlayers)
    let _projectiles_ = FSharp.Data.Adaptive.cset(value.projectiles)
    let _currentHp_ = FSharp.Data.Adaptive.cval(value.currentHp)
    let _maxHp_ = FSharp.Data.Adaptive.cval(value.maxHp)
    let _hitAnimations_ = FSharp.Data.Adaptive.cset(value.hitAnimations)
    let _explosionAnimations_ = FSharp.Data.Adaptive.cset(value.explosionAnimations)
    let _triggerHeld_ = FSharp.Data.Adaptive.cval(value.triggerHeld)
    let _killfeed_ = FSharp.Data.Adaptive.cval(value.killfeed)
    let _gotHitIndicatorInstances_ = FSharp.Data.Adaptive.cset(value.gotHitIndicatorInstances)
    let _hitEnemyIndicatorInstances_ = FSharp.Data.Adaptive.cmap(value.hitEnemyIndicatorInstances)
    let _lastPositionReset_ = FSharp.Data.Adaptive.cval(value.lastPositionReset)
    let _lastGotHit_ = FSharp.Data.Adaptive.cval(value.lastGotHit)
    let _deathTime_ = FSharp.Data.Adaptive.cval(value.deathTime)
    let _gameEndTime_ = FSharp.Data.Adaptive.cval(value.gameEndTime)
    let _gameStartTime_ = FSharp.Data.Adaptive.cval(value.gameStartTime)
    let _serverTime_ = FSharp.Data.Adaptive.cval(value.serverTime)
    let _tabDown_ = FSharp.Data.Adaptive.cval(value.tabDown)
    let _ctrlDown_ = FSharp.Data.Adaptive.cval(value.ctrlDown)
    let _shiftDown_ = FSharp.Data.Adaptive.cval(value.shiftDown)
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Model) = AdaptiveModel(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Model) -> AdaptiveModel(value)) (fun (adaptive : AdaptiveModel) (value : Model) -> adaptive.Update(value))
    member __.Update(value : Model) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Model>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _targets_.Value <- value.targets
            _onFloor_.Value <- value.onFloor
            _time_.Value <- value.time
            _lastDt_.Value <- value.lastDt
            _size_.Value <- value.size
            _camera_.Update(value.camera)
            _proj_.Value <- value.proj
            _isZoomed_.Value <- value.isZoomed
            _playerName_.Value <- value.playerName
            _frags_.Value <- value.frags
            _deaths_.Value <- value.deaths
            _color_.Value <- value.color
            _moveSpeed_.Value <- value.moveSpeed
            _airAccel_.Value <- value.airAccel
            _weapons_.Value <- value.weapons
            _activeWeapon_.Value <- value.activeWeapon
            _shotTrails_.Value <- value.shotTrails
            _gunAnimationState_.Value <- value.gunAnimationState
            _otherPlayers_.Value <- value.otherPlayers
            _projectiles_.Value <- value.projectiles
            _currentHp_.Value <- value.currentHp
            _maxHp_.Value <- value.maxHp
            _hitAnimations_.Value <- value.hitAnimations
            _explosionAnimations_.Value <- value.explosionAnimations
            _triggerHeld_.Value <- value.triggerHeld
            _killfeed_.Value <- value.killfeed
            _gotHitIndicatorInstances_.Value <- value.gotHitIndicatorInstances
            _hitEnemyIndicatorInstances_.Value <- value.hitEnemyIndicatorInstances
            _lastPositionReset_.Value <- value.lastPositionReset
            _lastGotHit_.Value <- value.lastGotHit
            _deathTime_.Value <- value.deathTime
            _gameEndTime_.Value <- value.gameEndTime
            _gameStartTime_.Value <- value.gameStartTime
            _serverTime_.Value <- value.serverTime
            _tabDown_.Value <- value.tabDown
            _ctrlDown_.Value <- value.ctrlDown
            _shiftDown_.Value <- value.shiftDown
    member __.Current = __adaptive
    member __.world = __value.world
    member __.targets = _targets_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.string, Aardwars.Target>
    member __.onFloor = _onFloor_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>
    member __.time = _time_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.lastDt = _lastDt_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.size = _size_ :> FSharp.Data.Adaptive.aval<Aardvark.Base.V2i>
    member __.camera = _camera_
    member __.proj = _proj_ :> FSharp.Data.Adaptive.aval<Aardvark.Rendering.Frustum>
    member __.isZoomed = _isZoomed_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>
    member __.playerName = _playerName_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>
    member __.frags = _frags_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    member __.deaths = _deaths_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.int>
    member __.color = _color_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.string>
    member __.moveSpeed = _moveSpeed_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.airAccel = _airAccel_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.weapons = _weapons_ :> FSharp.Data.Adaptive.amap<Aardwars.WeaponType, Aardwars.Weapon>
    member __.activeWeapon = _activeWeapon_ :> FSharp.Data.Adaptive.aval<Aardwars.WeaponType>
    member __.shotTrails = _shotTrails_ :> FSharp.Data.Adaptive.aset<Aardwars.TrailInfo>
    member __.gunAnimationState = _gunAnimationState_ :> FSharp.Data.Adaptive.aval<AnimationState>
    member __.otherPlayers = _otherPlayers_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.string, Aardwars.OtherPlayerInfo>
    member __.projectiles = _projectiles_ :> FSharp.Data.Adaptive.aset<ProjectileInfo>
    member __.currentHp = _currentHp_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.maxHp = _maxHp_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.hitAnimations = _hitAnimations_ :> FSharp.Data.Adaptive.aset<HitAnimation>
    member __.explosionAnimations = _explosionAnimations_ :> FSharp.Data.Adaptive.aset<ExplosionAnimationInfo>
    member __.triggerHeld = _triggerHeld_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>
    member __.killfeed = _killfeed_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Collections.list<(Microsoft.FSharp.Core.float * Microsoft.FSharp.Core.string)>>
    member __.gotHitIndicatorInstances = _gotHitIndicatorInstances_ :> FSharp.Data.Adaptive.aset<GotHitIndicatorInstance>
    member __.hitEnemyIndicatorInstances = _hitEnemyIndicatorInstances_ :> FSharp.Data.Adaptive.amap<Microsoft.FSharp.Core.string, Microsoft.FSharp.Core.float>
    member __.lastPositionReset = _lastPositionReset_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.lastGotHit = _lastGotHit_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.Option<(Microsoft.FSharp.Core.string * Aardwars.WeaponType)>>
    member __.deathTime = _deathTime_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.Option<Microsoft.FSharp.Core.float>>
    member __.gameEndTime = _gameEndTime_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.Option<Microsoft.FSharp.Core.float>>
    member __.gameStartTime = _gameStartTime_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.serverTime = _serverTime_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.tabDown = _tabDown_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>
    member __.ctrlDown = _ctrlDown_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>
    member __.shiftDown = _shiftDown_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>

