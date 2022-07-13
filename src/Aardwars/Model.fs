namespace Elm

open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Application
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Base
open Aardwars
open Aardvark.Rendering.Text

type World =
    {
        Hit : V3d -> V3d -> V3d * bool
        Scene : IRenderWindow -> ISg
    }

module Shader =
    open FShade

    type Vertex =
        {
            [<Semantic("Offset")>] o : V4d
            [<Position>] p : V4d
        }

    let instanceOffset (v : Vertex) =
        vertex {
            return { v with p = V4d(v.o.XYZ + v.p.XYZ, v.p.W) }
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
    
    let boxWorld (infos : BoxInfo[]) (playerHeight : float)=
        let tree = BoxKdTree(infos)

        let existing = infos |> Array.map (fun i -> i.Offset) |> Aardvark.Base.HashSet.ofArray

        let renderOffsets =
            infos |> Array.choose (fun i ->
                let occluded = 
                    existing.Contains(i.Offset + V3i.OOI) &&
                    existing.Contains(i.Offset - V3i.OOI) &&
                    existing.Contains(i.Offset + V3i.IOO) &&
                    existing.Contains(i.Offset - V3i.IOO) &&
                    existing.Contains(i.Offset + V3i.OIO) &&
                    existing.Contains(i.Offset - V3i.OIO)
                if occluded then
                    None
                else
                    Some (V4f(float32 i.Offset.X, float32 i.Offset.Y, float32 i.Offset.Z, 0.0f))
            )


        let playOffset = V3d(0.0, 0.0, playerHeight)
        let bb = IndexedGeometryPrimitives.Box.solidBox Box3d.Unit C4b.White
        let pos = bb.IndexedAttributes.[DefaultSemantic.Positions] :?> V3f[]
        let ns = bb.IndexedAttributes.[DefaultSemantic.Normals] :?> V3f[]
        let idx = bb.IndexArray :?> int[]
        { 
            Hit = fun p0 p1 -> 
                let (p1, f) = tree.Hit(p0 - playOffset, p1 - playOffset)
                p1 + playOffset, f
            Scene = fun win ->
                Sg.render IndexedGeometryMode.TriangleList (DrawCallInfo(FaceVertexCount = idx.Length, InstanceCount = renderOffsets.Length))
                |> Sg.vertexAttribute' DefaultSemantic.Positions pos
                |> Sg.vertexAttribute' DefaultSemantic.Normals ns
                |> Sg.index' idx
                |> Sg.instanceAttribute' "Offset" renderOffsets
                |> Sg.shader {
                    do! Shader.instanceOffset
                    do! DefaultSurfaces.trafo
                    do! DefaultSurfaces.constantColor C4f.Red
                    do! DefaultSurfaces.simpleLighting
                }
        }

    let randomBoxWorld (blockCount : int) (gridSize : V2i) (playerHeight : float) =
        let grid =
            let rand = RandomSystem()
            let grid = Array2D.create gridSize.X gridSize.Y 1
            for i in 0 .. blockCount - 1 do
                let x = int <| clamp 0.0 (float gridSize.X - 1.0) (round <| rand.Gaussian(float gridSize.X/2.0, float gridSize.X/4.0))
                let y = int <| clamp 0.0 (float gridSize.Y - 1.0) (round <| rand.Gaussian(float gridSize.Y/2.0, float gridSize.Y/4.0))

                grid.[x,y] <- grid.[x,y] + 1
            grid
    

        let infos = 
            [|
                for x in 0 .. gridSize.X - 1 do
                    for y in 0 .. gridSize.Y - 1 do
                        let v = grid.[x,y]
                        for z in 0 .. v - 1 do 
                            yield { Offset = V3i(x,y,z); Material = "thing" }
            |]

        boxWorld infos playerHeight
        


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
                Trafo3d.RotationYInDegrees(90.0) *
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
    }


