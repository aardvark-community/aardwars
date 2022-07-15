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
        Bounds : Box3d
        Hit : V3d -> V3d -> V3d * bool
        Scene : IRenderWindow -> ISg
    }

module Shader =
    open FShade

    type UniformScope with
        member x.Offset : V4d[] = uniform?StorageBuffer?Offset
        member x.Scale : V4d[] = uniform?StorageBuffer?Scale

    type Vertex =
        {
            [<Position>]
            pos : V4d

            [<Semantic("VertexPos")>]
            vert : V4d
            
            [<Normal>]
            n : V3d
            
            [<Semantic("Offset")>]
            off : V4d

            [<Semantic("Scale")>]
            scale : V4d
            
            [<Semantic("TextureIds"); Interpolation(InterpolationMode.Flat)>]
            texIds : V3i

        }

    let boxy (v : Vertex) =
        vertex {
            let bb = Bitwise.FloatBitsToUInt v.off.W
            let top = bb >>> 16
            let bottom = bb &&& 0xFFFFu
            let side = v.scale.W |> int

            return { 
                v with 
                    vert = v.pos
                    pos = V4d(v.off.XYZ + v.scale.XYZ * v.pos.XYZ, v.pos.W) 
                    texIds = V3i(int bottom, int top, side)
            }
        }

    let sammy =
        sampler2dArray {
            texture uniform?Atlas
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
            filter Filter.MinLinearMagPointMipLinear
        }
        
    let texy (v : Vertex) =
        fragment {
            let color = 
                if v.n.Z > 0.5 then sammy.Sample(v.vert.XY, v.texIds.Y)
                elif v.n.Z < -0.5 then sammy.Sample(v.vert.XY, v.texIds.X)
                elif abs v.n.X > 0.5 then sammy.Sample(v.vert.YZ, v.texIds.Z)
                else sammy.Sample(v.vert.XZ, v.texIds.Z)
            if color.W < 0.05 then discard()
            return color
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
            Bounds = Box3d.FromCenterAndSize(V3d.Zero, V3d(200.0, 200.0, 1.0))
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
            Bounds = Box3d worldBoxes
        }
    
    let treeWorld (win : IRenderWindow) (regionsDir : string) (assetDir : string) (playerHeight : float) =
        let (tree,tex) = MinecraftWorld.load win.Runtime regionsDir assetDir
        let hit (p0 : V3d) (p1 : V3d) =
            let p0 = p0 - (V3d.OOI * playerHeight)
            let mutable p1 = p1 - (V3d.OOI * playerHeight)

            
            let mutable zUp = false
            let tryRepair p1 =
                tree.GetIntersecting(Sphere3d(p1, 2.0).BoundingBox3d) 
                |> Seq.fold (fun p1 b ->    
                    let b = 
                        let b = b.BoundingBox
                        Box3d.FromCenterAndSize(b.Center, 1.05 * b.Size)
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
                else newP1.Z <= tree.BoundingBox.Min.Z + playerHeight
            newP1,onFloor

        let inst : LodTreeInstance = 
            { 
                root = OctRenderNode(tree.Root)
                uniforms = MapExt.empty
            }

        let cfg : LodTreeRenderConfig =
            {
                budget = AVal.constant 500000
                splitfactor = AVal.constant 0.05
                maxSplits = AVal.constant 32
                time = win.Time
                renderBounds = AVal.constant false
                stats = cval Unchecked.defaultof<_>
                pickTrees = None
                alphaToCoverage = false
            }

        let sg = 
            Sg.lodTree cfg (ASet.single inst)
            |> Sg.shader {
                do! Shader.boxy
                do! DefaultSurfaces.trafo
                do! Shader.texy
                do! DefaultSurfaces.simpleLighting
            }   
            |> Sg.texture' "Atlas" tex
        {
            Hit = hit
            Scene = fun _ -> sg
            Bounds = tree.BoundingBox
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

type TrailInfo = 
    {
        Line        :  Line3d
        startTime   :  float
        duration    :  float
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
        shotTrails  : HashSet<TrailInfo>

    }


