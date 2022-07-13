namespace Aardwars

open Aardvark.Base
open Aardvark.Base.Sorting

[<Struct>]
type BoxInfo =
    {
        Offset : V3i
        Material : string
    }

type BoxKdNode =
    | Leaf of Box3d * struct(Box3d * int)[]
    | Node of Box3d * splitAxis : int * splitValue : float * left : BoxKdNode * right : BoxKdNode

module BoxKdNode =
    let rec build (leafLimit : int) (boxes : struct(Box3d * int)[]) (bounds : Box3d) =
        if boxes.Length <= leafLimit then
            Leaf(bounds, boxes)
        else
            let m = boxes.Length / 2
            let mutable bestDim = -1
            let mutable bestValue = 0.0
            let mutable bestLeft = null
            let mutable bestRight = null
            let mutable bestLeftBounds = Unchecked.defaultof<_>
            let mutable bestRightBounds = Unchecked.defaultof<_>
            let mutable bestScore = System.Double.PositiveInfinity
            for dim in 0 .. 2 do
                let perm = boxes.CreatePermutationQuickMedianAscending((fun struct(b : Box3d,_) -> b.Min.[dim]), m)

                let mv = 
                    let struct(b, _) = boxes.[perm.[m]]
                    b.Min.[dim]

                let left, right = boxes |> Array.partition(fun struct(b,_) -> b.Max.[dim] <= mv)
                let lb = (Box3d.Invalid, left) ||> Array.fold (fun (b : Box3d) struct(c, _) -> b.ExtendedBy c)
                let rb = (Box3d.Invalid, right) ||> Array.fold (fun (b : Box3d) struct(c, _) -> b.ExtendedBy c)

                let score = (lb.SurfaceArea / bounds.SurfaceArea) * float left.Length + (rb.SurfaceArea / bounds.SurfaceArea) * float right.Length
                if score < bestScore then
                    bestScore <- score
                    bestDim <- dim
                    bestValue <- mv
                    bestLeft <- left
                    bestRight <- right
                    bestLeftBounds <- lb
                    bestRightBounds <-  rb

            if System.Double.IsInfinity bestScore then printfn "sadlkasmdksamdk"

            let ln = build leafLimit bestLeft bestLeftBounds
            let rn = build leafLimit bestRight bestRightBounds
            Node(bounds, bestDim, bestValue, ln, rn)

    let rec queryBox (query : Box3d) (node : BoxKdNode) =
        match node with
        | Leaf(_, boxes) ->
            boxes |> Seq.choose (fun struct(b, idx) -> 
                if query.Intersects b then Some (b, idx)
                else None
            )
        | Node(bb, sd, sv, l, r) ->
            if bb.Intersects query then
                if query.Max.[sd] <= sv then
                    queryBox query l
                elif query.Min.[sd] >= sv then
                    queryBox query r
                else
                    Seq.append 
                        (queryBox query l) 
                        (Seq.delay (fun () -> queryBox query r))
            else
                Seq.empty
            
    let rec queryRay (ray : FastRay3d) (tmin : float) (tmax : float) (node : BoxKdNode) =
        match node with
        | Leaf(_, boxes) ->
            let mutable hit = None
            let mutable closestT = tmax
            for struct(box, idx) in boxes do
                let mutable t0 = 0.0
                let mutable t1 = 0.0
                if ray.Intersects(box, &t0, &t1) then
                    if t0 >= tmin && t0 <= closestT then
                        closestT <- t0
                        hit <- Some (box, idx, t0)
            hit
        | Node(bb, sd, sv, l, r) ->
            let mutable t0 = 0.0
            let mutable t1 = 0.0
            if ray.Intersects(bb, &t0, &t1) then
                let tmin = max t0 tmin
                let tmax = min t1 tmax
                if tmin < tmax then
                    let plane = 
                        let mutable n = V3d.Zero
                        n.[sd] <- 1.0
                        Plane3d(n, sv)

                    let mutable ts = 0.0


                    if ray.Ray.Intersects(plane, &ts) && ts >= tmin && ts <= tmax then
                        let mutable near = l
                        let mutable far = r
                        if ray.Ray.Direction.[sd] < 0.0 then Fun.Swap(&near, &far)

                        match queryRay ray tmin ts near with
                        | Some hit -> Some hit
                        | None -> queryRay ray ts tmax far

                    else
                        let o = ray.Ray.GetPointOnRay(tmin)
                        if o.[sd] < sv then queryRay ray tmin tmax l
                        else queryRay ray tmin tmax r
                else
                    None
            else
                None


type BoxKdTree(boxes : BoxInfo[]) =
    let root = 
        let mutable bounds = Box3d.Invalid
        let bbs =
            boxes |> Array.mapi (fun i b ->
                let b = Box3d.FromMinAndSize(V3d b.Offset, V3d.III)
                bounds.ExtendBy b
                struct(b, i)
            )
        BoxKdNode.build 16 bbs bounds


    member x.Intersects(ray : Ray3d) =
        let f = FastRay3d(ray)
        match BoxKdNode.queryRay f 0.0 System.Double.PositiveInfinity root with
        | Some (b, idx, t) ->
            Some (t, boxes.[idx])
        | None ->
            None
            
    member x.Hit(p0 : V3d, p1 : V3d) =
        let radius = 0.2
        let s = Sphere3d(p1, radius)
        let boxes = BoxKdNode.queryBox s.BoundingBox3d root |> Seq.toArray

        let mutable p1 = p1

        let mutable zUp = false
        let tryRepair p1 =
            boxes 
            |> Array.fold (fun p1 (b,_) ->
                let pos = Motion.repairMotion p0 p1 b radius
                match pos with
                | Some (z, p) -> 
                    if z then zUp <- true
                    p
                | None -> p1
            ) p1
        let p1 = tryRepair p1
        let newP1 = p1
        let onFloor =                        
            if zUp then true
            else newP1.Z <= 0.0
        newP1,onFloor
