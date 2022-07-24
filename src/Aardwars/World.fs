namespace Aardwars

open FSharp.Data.Adaptive
open Adaptify
open Aardvark.Application
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Base
open Aardvark.Rendering.Text

type World =
    {
        Bounds : Box3d
        Hit : V3d -> V3d -> V3d * bool
        Intersections : Ray3d -> float -> float -> seq<float * BoxInfo>
        Scene : IRenderWindow -> ISg
        RespawnLocation : unit -> V3d
    }

module World =
    module Impl = 
        
        let hitBox (ray : Ray3d) (tmin : float) (tmax : float) (box : Box3d) =
            // o.X + t*d.X = box.Min.X
            let mutable near = V3d.Zero
            let mutable far = V3d.Zero
            box.GetMinMaxInDirection(ray.Direction, &near, &far)

            let ts = (near - ray.Origin) / ray.Direction

            let mutable v = -1.0
            let mutable hits = []

            if ts.X >= tmin && ts.X <= tmax then
                let yz = ray.Origin.YZ + ts.X * ray.Direction.YZ
                if yz.AllGreaterOrEqual box.Min.YZ && yz.AllSmallerOrEqual box.Max.YZ then
                    // real intersection

                    if abs ray.Direction.X > v then
                        v <- abs ray.Direction.X 
                        let hit = 
                            if near.X = box.Min.X then ts.X, Plane3d(-V3d.IOO, -box.Min.X)
                            else ts.X, Plane3d(V3d.IOO, box.Max.X)
                        hits <- [hit]

            if ts.Y >= tmin && ts.Y <= tmax then
                let xz = ray.Origin.XZ + ts.X * ray.Direction.XZ
                if xz.AllGreaterOrEqual box.Min.XZ && xz.AllSmallerOrEqual box.Max.XZ then
                    // real intersection
                    if abs ray.Direction.Y > v then
                        v <- abs ray.Direction.Y
                        let hit = 
                            if near.Y = box.Min.Y then ts.Y, Plane3d(-V3d.OIO, -box.Min.Y)
                            else ts.Y, Plane3d(V3d.OIO, box.Max.Y)
                        hits <- [hit]
                
            if ts.Z >= tmin && ts.Z <= tmax then
                let xy = ray.Origin.XY + ts.X * ray.Direction.XY
                if xy.AllGreaterOrEqual box.Min.XY && xy.AllSmallerOrEqual box.Max.XY then
                    // real intersection
                    if abs ray.Direction.Z > v then
                        v <- abs ray.Direction.Z
                        let hit = 
                            if near.Z = box.Min.Z then ts.Z, Plane3d(-V3d.OOI, -box.Min.Z)
                            else ts.Z, Plane3d(V3d.OOI, box.Max.Z)
                        hits <- [hit]
            hits
    open Impl
    
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
        let respawn() =
            V3d.OOI * 5.0
        {
            Hit = hit
            Scene = fun _ -> sg
            Intersections = fun _ _ _ -> Seq.empty
            Bounds = Box3d.FromCenterAndSize(V3d.Zero, V3d(200.0, 200.0, 1.0))
            RespawnLocation = respawn
        }

    let minecraft (win : IRenderWindow) (tex : ITexture) (tree : Octree<BoxInfo>) (playerHeight : float) =
        let eps = 1E-3
        let onFloor(p : V3d) =
            let ray = Ray3d(p, -V3d.OOI)
            let res = Octree.rayIntersections ray 0.0 1E-2 tree |> Seq.isEmpty
            not res

        let inline ret (p : V3d) =
            p
             
        let rec realHit (iter : int) (p0 : V3d) (p1 : V3d) =
            if iter > 10 then
                Log.error "BAD"
                ret (p1 + V3d.OOI * 40.0)
            else
                let d = p1 - p0
                let len = Vec.length d
                if Fun.IsTiny len then
                    ret p1
                else
                    let ray = Ray3d(p0, d / len)
                    let hits = Octree.rayIntersections ray 0.0 (len + eps) tree |> Seq.truncate 1 |> Seq.toArray
                    if hits.Length > 0 then
                        let planes = 
                            hits |> Array.map (fun (t, b) ->
                                let bb = b.BoundingBox
                                let pt = ray.GetPointOnRay(t)
                                let pt = pt.GetClosestPointOn(bb)

                                if pt.Z = bb.Max.Z then Plane3d(V3d.OOI, bb.Max.Z)
                                elif pt.Z = bb.Min.Z then Plane3d(-V3d.OOI, -bb.Min.Z)
                                elif pt.Y = bb.Max.Y then Plane3d(V3d.OIO, bb.Max.Y)
                                elif pt.Y = bb.Min.Y then Plane3d(-V3d.OIO, -bb.Min.Y)
                                elif pt.X = bb.Max.X then Plane3d(V3d.IOO, bb.Max.X)
                                elif pt.X = bb.Min.X then Plane3d(-V3d.IOO, -bb.Min.X)
                                else Log.error "asdsadsad"; Plane3d.Invalid
                            )
                            |> Array.distinct

                        let offsetPlane (p : Plane3d) =
                            Plane3d(p.Normal, p.Distance + eps)


                        let p0', p1' = 
                            match planes.Length with
                            | 0 -> 
                                Log.error "0 face: %0A" hits
                                p1, p1
                            | 1 ->
                                Log.line "1 face: %0A" planes
                                let plane = offsetPlane planes.[0] 
                                let p0 = ray.Intersect plane
                                let p1 = p1.GetClosestPointOn plane
                                p0, p1
                            | 2 ->
                                Log.line "2 faces: %0A" planes
                                let plane0 = offsetPlane planes.[0] 
                                let plane1 = offsetPlane planes.[1]
                                

                                let mutable r = Unchecked.defaultof<_>
                                if plane0.Intersects(plane1, &r) then
                                    let mutable t0 = 0.0
                                    let mutable t1 = 0.0
                                    ray.Intersects(plane0, &t0) |> ignore
                                    ray.Intersects(plane1, &t1) |> ignore

                                    let tmin = 
                                        if t0 < t1 then t0
                                        else t1

                                    let p0 = ray.GetPointOnRay tmin

                                    let p1 = p1.GetClosestPointOn(r)
                                    p0, p1
                                else 
                                    failwith ""


                            | 3 ->
                                Log.line "3 faces: %0A" planes
                                let plane0 = offsetPlane planes.[0] 
                                let plane1 = offsetPlane planes.[1]
                                let plane2 = offsetPlane planes.[2] 
                                let mutable t0 = 0.0
                                let mutable t1 = 0.0
                                let mutable t2 = 0.0
                                ray.Intersects(plane0, &t0) |> ignore
                                ray.Intersects(plane1, &t1) |> ignore
                                ray.Intersects(plane1, &t2) |> ignore

                                let tmin = Array.min [| t0; t1; t2 |]
                                let p0 = ray.GetPointOnRay tmin
                                let mutable pt = V3d.Zero
                                if plane0.Intersects(plane1, plane2, &pt) then
                                    p0, pt
                                else
                                    Log.error "no intersection point"
                                    p0, p1
                            | _ ->
                                Log.error "too many"
                                p0, p0

                        if Fun.ApproximateEquals(p0', p1', 1E-8) then
                            ret p0'
                        else
                            realHit (iter + 1) p0' p1'

                    else
                        ret p1

        let hit (p0 : V3d) (p1 : V3d) =
            if Fun.ApproximateEquals(p0, p1) then
                p1, onFloor p1
            else
                let b0 = Box3d(V3d(p0.X - 0.3, p0.Y - 0.3, p0.Z - playerHeight), V3d(p0.X + 0.3, p0.Y + 0.3, p0.Z))
                let b1 = Box3d(V3d(p1.X - 0.3, p1.Y - 0.3, p1.Z - playerHeight), V3d(p1.X + 0.3, p1.Y + 0.3, p1.Z))
                let d = p1 - p0
                let mutable b1 = b1
                let mutable badBox = Octree.overlapping b1 tree |> Seq.filter (fun b -> b.RenderStyle <> RenderStyle.Cross) |> Seq.tryHead
                while Option.isSome badBox do
                    let bad = badBox.Value.BoundingBox

                    let dx =
                        if d.X > 0.0 then bad.Min.X - b1.Max.X
                        elif d.X < 0.0 then bad.Max.X - b1.Min.X
                        else System.Double.PositiveInfinity
                            
                    let dy =
                        if d.Y > 0.0 then bad.Min.Y - b1.Max.Y
                        elif d.Y < 0.0 then bad.Max.Y - b1.Min.Y
                        else System.Double.PositiveInfinity
                                
                    let dz =
                        if d.Z > 0.0 then bad.Min.Z - b1.Max.Z
                        elif d.Z < 0.0 then bad.Max.Z - b1.Min.Z
                        else System.Double.PositiveInfinity
                            
                    let dir =
                        let eps v =
                            if v < 0.0 then v - 1E-5
                            else v + 1E-5
                        if abs dx < abs dy && abs dx < abs dz then V3d(eps dx, 0.0, 0.0)
                        elif abs dy < abs dx && abs dy < abs dz then V3d(0.0, eps dy, 0.0)
                        else V3d(0.0, 0.0, eps dz)

                    b1 <- b1.Translated dir

                    if b1.Intersects bad then 
                        Log.error "wrong"

                    badBox <- Octree.overlapping b1 tree |> Seq.filter (fun b -> b.RenderStyle <> RenderStyle.Cross) |> Seq.tryHead

                let p1 = b1.Max - V3d(0.3, 0.3, 0.0)
                p1, onFloor(p1 - V3d(0.0, 0.0, playerHeight))

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
                do! Shader.texy
                do! DefaultSurfaces.simpleLighting
                do! Shader.foggy
            }   
            |> Sg.texture' "Atlas" tex
            |> Sg.cullMode' CullMode.Back
        let bounds = tree.BoundingBox
        let rand = RandomSystem()
        let inline randomBetween mi ma =
            let r = rand.UniformDouble()
            let range = ma - mi
            mi + range * r
        let spawns = 
            [|
                V3d(20.0,-64.5,82.0)
                V3d(63.5,-51.0,78.0)
                V3d(80.5,-41.0,85.0)
                V3d(84.5,-8.5,72.5)
                V3d(72.5,25.5,80.5)
                V3d(52.5,48.0,87.0)
                V3d(43.5,74.0,93.0)
                V3d(-1.5,90.0,86.0)
                V3d(-44.0,98.5,90.5)
                V3d(-70.5,76.5,76.0)
                V3d(-90.0,63.0,85.5)
                V3d(-72.0,-6.5,92.0)
                V3d(-72.0,-6.5,92.0)
                V3d(-60.0,-43.0,80.0)
                V3d(-47.0,-63.0,80.0)
                V3d(7.0,-65.0,83.0)
                V3d(-39.0,-64.0,74.0)
            |]
        let respawnLocation() = spawns.[rand.UniformInt(spawns.Length-1)]
        {
            Hit = hit
            Intersections = fun r tmin tmax -> Octree.rayIntersections r tmin tmax tree
            Scene = fun _ -> sg
            Bounds = bounds
            RespawnLocation = respawnLocation
        }