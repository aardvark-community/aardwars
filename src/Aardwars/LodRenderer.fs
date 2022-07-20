namespace Aardwars

open Aardvark.Base
open Aardvark.Base.Sorting
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.SceneGraph
open Microsoft.FSharp.NativeInterop

#nowarn "9"

type V3s =
    struct
        val mutable public X : int16
        val mutable public Y : int16
        val mutable public Z : int16

        
        override x.ToString() = sprintf "[%d, %d, %d]" x.X x.Y x.Z

        new(x : int16, y : int16, z : int16) = { X = x; Y = y; Z = z }
        new(x : int, y : int, z : int) = { X = int16 x; Y = int16 y; Z = int16 z }

        static member (+) (a : V3s, b : V3s) = V3s(a.X + b.X, a.Y + b.Y, a.Z + b.Z)
        static member (*) (a : V3s, b : int16) = V3s(a.X * b, a.Y * b, a.Z * b)

    end

type RenderStyle =
    | Box       = 0us
    | Cross     = 1us

[<Struct; StructuredFormatDisplay("{AsString}")>]
type BoxInfo =
    {
        Offset : V3s
        MaterialId : V3s
        RenderStyle : RenderStyle
    }
    member private x.AsString = x.ToString()

    override x.ToString() = string x.Offset

    member x.BoundingBox =
        let o = V3d(float x.Offset.X, float x.Offset.Y, float x.Offset.Z)
        Box3d(o, o + V3d.III)



type OctNode<'a> =
    | Empty
    | Leaf of cell : Cell * bounds : Box3d * count : int * elements : list<'a>
    | Node of cell : Cell * bounds : Box3d * count : int * OctNode<'a>[]
    
    member x.Bounds =
        match x with
        | Empty -> Box3d.Invalid
        | Leaf(_,b,_,_) -> b
        | Node(_,b,_,_) -> b

type Block =
    | All of texture : string
    | BottomTop of bottom : string * top : string * side : string
    | Cross of texture : string
    | Unknown

module Block =
    open System.IO
    open System.Text.Json

    let pp (dir : string) (str : string) =  
        let name =
            let idx = str.IndexOf ":" 
            if idx >= 0 then str.Substring(idx + 1)
            else str

        let name = Path.GetFileNameWithoutExtension(name.Replace('/', System.IO.Path.DirectorySeparatorChar))
        Path.Combine(dir, name + ".png")

    let parse (dir : string)  (json : string) =
        let d = JsonDocument.Parse(json)
        let tex = d.RootElement.GetProperty("textures")

        match tex.TryGetProperty "all" with
        | (true, all) -> All (all.GetString() |> pp dir)
        | _ ->
            match tex.TryGetProperty "top", tex.TryGetProperty "side", tex.TryGetProperty "bottom" with
            | (true, top), (true, side), (true, bottom) ->
                BottomTop(bottom.GetString() |> pp dir, top.GetString() |> pp dir, side.GetString() |> pp dir)
            | _ ->
                match tex.TryGetProperty "cross" with
                | (true, c) -> 
                    Cross (c.GetString() |> pp dir)
                | _ ->
                    match tex.TryGetProperty "particle" with
                    | (true, p) -> All (p.GetString() |> pp dir)
                    | _ ->
                        Unknown

    let loadMapping (dir : string) =
        let table = Dict<string, Block>()
        for f in Directory.GetFiles(dir, "*.json") do
            let name = Path.GetFileNameWithoutExtension f
            try 
                let b = parse dir (File.ReadAllText f)
                table.["minecraft:" + name] <- b
            with _ ->
                ()
        table

type OctRenderNode(level : int, root : option<ILodTreeNode>, parent : option<ILodTreeNode>, node : OctNode<BoxInfo>) as this =
    static let geometry =

        let pos =
            [|
                V3f.OOO; V3f.IIO; V3f.IOO; V3f.OOO; V3f.OIO; V3f.IIO // -z
                V3f.OOI; V3f.IOI; V3f.III; V3f.OOI; V3f.III; V3f.OII // +z

                V3f.OOO; V3f.IOO; V3f.IOI; V3f.OOO; V3f.IOI; V3f.OOI // -y
                V3f.OIO; V3f.III; V3f.IIO; V3f.OIO; V3f.OII; V3f.III // +y
                
                V3f.OOO; V3f.OII; V3f.OIO; V3f.OOO; V3f.OOI; V3f.OII // -x
                V3f.IOO; V3f.IIO; V3f.III; V3f.IOO; V3f.III; V3f.IOI // +x
            |]

        let ns =
            [|
                V3f.OON; V3f.OON; V3f.OON; V3f.OON; V3f.OON; V3f.OON
                V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI; V3f.OOI
                
                V3f.ONO; V3f.ONO; V3f.ONO; V3f.ONO; V3f.ONO; V3f.ONO
                V3f.OIO; V3f.OIO; V3f.OIO; V3f.OIO; V3f.OIO; V3f.OIO

                V3f.NOO; V3f.NOO; V3f.NOO; V3f.NOO; V3f.NOO; V3f.NOO
                V3f.IOO; V3f.IOO; V3f.IOO; V3f.IOO; V3f.IOO; V3f.IOO
            |]


        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,
            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, pos :> System.Array
                    DefaultSemantic.Normals, ns :> System.Array
                ]
        )

    static let crossGeometry =
        let pos =
            [| 
                V3f(0.0f, 0.5f, 0.0f); V3f(1.0f, 0.5f, 0.0f); V3f(1.0f, 0.5f, 1.0f)
                V3f(1.0f, 0.5f, 1.0f); V3f(0.0f, 0.5f, 0.0f); V3f(0.0f, 0.5f, 1.0f)
                
                V3f(0.5f, 0.0f, 0.0f); V3f(0.5f, 1.0f, 0.0f); V3f(0.5f, 1.0f, 1.0f)
                V3f(0.5f, 1.0f, 1.0f); V3f(0.5f, 0.0f, 0.0f); V3f(0.5f, 0.0f, 1.0f)
            |]
        let ns =
            [|
                V3f.OIO; V3f.OIO; V3f.OIO; V3f.OIO; V3f.OIO; V3f.OIO 
                V3f.IOO; V3f.IOO; V3f.IOO; V3f.IOO; V3f.IOO; V3f.IOO
            |]

        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,
            IndexedAttributes =
                SymDict.ofList [
                    DefaultSemantic.Positions, pos :> System.Array
                    DefaultSemantic.Normals, ns :> System.Array
                ]
        )
    
    let children = 
        lazy (
            let realRoot =
                match root with
                | Some r -> r
                | None -> this
            match node with
            | Node(_,_,_,e) -> 
                e |> Array.choose (fun n ->     
                    match n with
                    | Empty -> None
                    | _ -> Some (OctRenderNode(level + 1, Some realRoot, Some this, n) :> ILodTreeNode)
                )
            | _ ->
                [||]
        )
    let fov (proj : Trafo3d) =
        2.0 * atan(proj.Backward.M00) * Constant.DegreesPerRadian

    let isLeaf =
        match node with
        | Empty -> true
        | Leaf(_,_,_,_) -> true
        | _ -> false

    let localCellBounds =
        match node with
        | Empty -> Box3d.Unit
        | Leaf(_,b,_,_) -> b
        | Node(_,b,_,_) -> b

    let totalDataSize =
        match node with
        | Empty -> 0
        | Leaf(_,_,c,_) -> c
        | Node(_,_,c,_) -> c
    let bb =
        match node with
        | Empty -> Box3d.Invalid
        | Leaf(_,b,_,_) -> b
        | Node(_,b,_,_) -> b

    let equivalentAngle60 (view : Trafo3d) (proj : Trafo3d) =

        let cam = view.Backward.C3.XYZ

        let avgPointDistance = localCellBounds.Size.NormMax / 40.0

        let minDist = localCellBounds.GetMinimalDistanceTo(cam)
        let minDist = max 0.01 minDist

        let angle = Constant.DegreesPerRadian * atan2 avgPointDistance minDist

        let fov = fov proj

        60.0 * angle / fov 

    let dataSize =
        match node with
        | Empty -> 0
        | Leaf(_,_,c,_) -> c
        | Node _ -> 1

    let cell =
        match node with
        | Empty -> Cell.Unit
        | Leaf(c,_,_,_) -> c
        | Node(c,_,_,_) -> c

    interface ILodTreeNode with
        member this.Acquire() = ()
        member this.Cell = cell
        member this.Children = children.Value :> seq<_>

        member x.ShouldSplit (splitfactor : float, quality : float, view : Trafo3d, proj : Trafo3d) =
            not isLeaf && equivalentAngle60 view proj > splitfactor / quality

        member x.ShouldCollapse (splitfactor : float, quality : float, view : Trafo3d, proj : Trafo3d) =
            equivalentAngle60 view proj < (splitfactor * 0.75) / quality

        member x.SplitQuality (splitfactor : float, view : Trafo3d, proj : Trafo3d) =
            splitfactor / equivalentAngle60 view proj

        member x.CollapseQuality (splitfactor : float, view : Trafo3d, proj : Trafo3d) =
            (splitfactor * 0.75) / equivalentAngle60 view proj

        member this.GetData(ct, inputs) = 
            match node with
            | Empty ->
                geometry, 
                MapExt.ofList [
                    "Offset", Array.empty<V4f> :> System.Array
                    "Scale", Array.empty<V4f> :> System.Array
                ]
            | Leaf(_,_,_,es) ->
                let offsets = 
                    es |> List.map (fun e -> 
                        let f = 
                            (uint32 e.MaterialId.Y <<< 16) ||| (uint32 e.MaterialId.X) 
                            |> System.BitConverter.GetBytes 
                            |> System.BitConverter.ToSingle
                        V4f(float32 e.Offset.X, float32 e.Offset.Y, float32 e.Offset.Z, f)
                    ) |> List.toArray
                let scales =    
                    es |> List.map (fun e -> 
                        let f = 
                            (uint32 e.RenderStyle <<< 16) ||| (uint32 e.MaterialId.Z)
                            |> System.BitConverter.GetBytes 
                            |> System.BitConverter.ToSingle
                        V4f(1.0f, 1.0f, 1.0f, f)
                    ) |> List.toArray


                geometry, 
                MapExt.ofList [
                    "Offset", offsets :> System.Array
                    "Scale", scales :> System.Array
                ]
            | Node(_,bb,_,_) ->
                geometry, 
                MapExt.ofList [
                    "Offset", [| V4f(V3f bb.Min, 0.0f) |] :> System.Array
                    "Scale", [| V4f(V3f bb.Size, 0.0f) |] :> System.Array
                ]
        member this.Parent = parent
        member this.Root = 
            match root with
            | Some root -> root
            | None -> this

        member this.DataSize = dataSize

        member this.DataSource = DefaultSemantic.CreaseAngle
        member this.DataTrafo = Trafo3d.Identity
        member this.Id = this :> obj
        member this.Level = level
        member this.Name = "Hans"
        member this.Release() = ()
        member this.TotalDataSize = totalDataSize
        member this.WorldBoundingBox = bb
        member this.WorldCellBoundingBox = bb

    new(node : OctNode<BoxInfo>) =
        OctRenderNode(0, None, None, node)

type Octree<'a>(root : OctNode<'a>, bounds : Box3d, leafLimit : int, getBoundingBox : 'a -> Box3d) =
    member x.IsEmpty = 
        match root with
        | Empty -> true
        | _ -> false
    member x.Root = root
    member x.LeafLimit = leafLimit
    member x.BoundingBox = bounds
    member x.GetBoundingBox = getBoundingBox
    member x.Count =
        match root with
        | Empty -> 0
        | Leaf(_,_,c,_) -> c
        | Node(_,_,c,_) -> c

    member x.GetIntersecting(b : Box3d) =
        let rec findIntersecting (b : Box3d) (node : OctNode<'a>) =
            match node with
            | Empty -> Seq.empty
            | Leaf(_,bounds,_,e) ->
                if bounds.Intersects b then
                    e |> Seq.filter (fun e -> (getBoundingBox e).Intersects b)
                else 
                    Seq.empty
            | Node(_,bounds,_,c) ->
                if bounds.Intersects b then
                    c |> Seq.collect (findIntersecting b)
                else
                    Seq.empty

        findIntersecting b root




module Octree =
    let empty<'a> (limit : int) (getBoundingBox : 'a -> Box3d) = Octree<'a>(Empty, Box3d.Invalid, limit, getBoundingBox)

    module Seq =
        open System.Collections
        open System.Collections.Generic

        type MergedEnumerator<'k, 'v when 'k : comparison>(es : IEnumerator<'k * 'v>[]) =
            let mutable current = HashSet.ofArray es
            let mutable value = Unchecked.defaultof<_>
            let mutable initial = true

            member x.MoveNext() =
                if current.Count = 0 then
                    false
                else
                    let success = 
                        if initial then current |> HashSet.filter (fun c -> c.MoveNext())
                        else current
                    initial <- false

                    if success.Count = 0 then
                        current <- HashSet.empty
                        false
                    else
                        use mutable e = success.GetEnumerator()
                        e.MoveNext() |> ignore
                        let mutable bestThing = e.Current
                        let mutable (bestKey, bestValue) = bestThing.Current
                        while e.MoveNext() do
                            let thing = e.Current
                            let (k,v) = thing.Current
                            if k < bestKey then
                                bestThing <- thing
                                bestKey <- k
                                bestValue <- v

                        if not (bestThing.MoveNext()) then current <- HashSet.remove bestThing current
                        value <- (bestKey, bestValue)
                        true

            member x.Current = value

            member x.Reset() =
                current <- HashSet.ofArray es
                for e in es do e.Reset()
                initial <- true
                value <- Unchecked.defaultof<_>

            interface IEnumerator with
                member x.MoveNext() = x.MoveNext()
                member x.Current = x.Current :> obj
                member x.Reset() = x.Reset()

            interface IEnumerator<'k * 'v> with
                member x.Current = x.Current
                member x.Dispose() =
                    current <- HashSet.empty
                    initial <- false
                    value <- Unchecked.defaultof<_>
                
        type MergedEnumerable<'k, 'v when 'k : comparison>(things : seq<'k * 'v>[]) =
            
            member x.GetEnumerator() = new MergedEnumerator<'k, 'v>(things |> Array.map (fun e -> e.GetEnumerator()))

            interface IEnumerable with
                member x.GetEnumerator() = new MergedEnumerator<'k, 'v>(things |> Array.map (fun e -> e.GetEnumerator())) :> _
                
            interface IEnumerable<'k * 'v> with
                member x.GetEnumerator() = new MergedEnumerator<'k, 'v>(things |> Array.map (fun e -> e.GetEnumerator())) :> _

        let mergeSorted (things : #seq<seq<'k * 'v>>) =
            MergedEnumerable(Seq.toArray things) :> seq<_>




    let add (things : 'a[]) (tree : Octree<'a>) =
        if things.Length <= 0 then
            tree
        else
            let boxes = things |> Array.map tree.GetBoundingBox
            let bounds = Box3d boxes
            let c = Cell bounds

            let rec insertContained (things : 'a[]) (boxes : Box3d[]) (node : OctNode<'a>) : OctNode<'a> =
                match node with
                | Empty ->
                    failwith "shouldn't happen"
                | Leaf(cell, bounds, cnt, elements) ->
                    
                    let newCnt = cnt + boxes.Length
                    if newCnt <= tree.LeafLimit then
                        Leaf(cell, bounds.Union (Box3d boxes), newCnt, Array.toList things @ elements)
                    else
                        let c = cell.GetCenter()
                        
                        let subThings = Array.init 8 (fun _ -> System.Collections.Generic.List(things.Length))
                        let subBoxes = Array.init 8 (fun _ -> System.Collections.Generic.List(things.Length))
                        let subBounds = Array.create 8 Box3d.Invalid
                        for i in 0 .. things.Length - 1 do
                            let cc = boxes.[i].Center

                            let oct = // TODO: lookup cell impl
                                (if cc.Z > c.Z then 4 else 0) |||
                                (if cc.Y > c.Y then 2 else 0) |||
                                (if cc.X > c.X then 1 else 0) 

                            subThings.[oct].Add things.[i]
                            subBoxes.[oct].Add boxes.[i]
                            subBounds.[oct].ExtendBy boxes.[i]

                        for thing in elements do
                            let box = tree.GetBoundingBox thing
                            let cc = box.Center

                            let oct = // TODO: lookup cell impl
                                (if cc.Z > c.Z then 4 else 0) |||
                                (if cc.Y > c.Y then 2 else 0) |||
                                (if cc.X > c.X then 1 else 0) 

                            subThings.[oct].Add thing
                            subBoxes.[oct].Add box
                            subBounds.[oct].ExtendBy box
                            
                        let childCells = cell.Children
                        let children =
                            Array.init 8 (fun i ->
                                let things = subThings.[i].ToArray()
                                let boxes = subBoxes.[i].ToArray()
                                let bb = subBounds.[i]
                                if things.Length > 0 then
                                    Leaf(childCells.[i],bb, 0, []) |> insertContained things boxes 
                                else
                                    Empty
                            )
                        Node(cell, Box3d subBounds, cnt + boxes.Length, children)

                | Node(cell, bounds, cnt, children) ->
                    let c = cell.GetCenter()
                        
                    let subThings = Array.init 8 (fun _ -> System.Collections.Generic.List(things.Length))
                    let subBoxes = Array.init 8 (fun _ -> System.Collections.Generic.List(things.Length))
                    for i in 0 .. things.Length - 1 do
                        let cc = boxes.[i].Center

                        let oct = // TODO: lookup cell impl
                            (if cc.Z > c.Z then 4 else 0) |||
                            (if cc.Y > c.Y then 2 else 0) |||
                            (if cc.X > c.X then 1 else 0) 

                        subThings.[oct].Add things.[i]
                        subBoxes.[oct].Add boxes.[i]
                    
                    let newChildren = 
                        children |> Array.mapi (fun i c ->
                            let things = subThings.[i].ToArray()
                            let boxes = subBoxes.[i].ToArray()
                            if things.Length > 0 then 
                                match c with
                                | Empty -> Leaf(cell.Children.[i], Box3d.Invalid, 0, []) |> insertContained things boxes
                                | c -> insertContained things boxes c
                            else c
                        )   
                    let newBounds = newChildren |> Array.map (fun c -> c.Bounds) |> Box3d
                    Node(cell, newBounds, cnt + boxes.Length, newChildren)

            match tree.Root with
            | Empty -> 
                let newRoot = 
                    Leaf(c, Box3d.Invalid, 0, [])
                    |> insertContained things boxes 
                Octree(newRoot, tree.BoundingBox.Union bounds, tree.LeafLimit, tree.GetBoundingBox)

            | Leaf(rootCell, rootBounds, cnt, elements) ->
                let newRootCell = Cell.GetCommonRoot(rootCell, c)
                let newRoot =
                    Leaf(newRootCell, rootBounds, cnt, elements)
                    |> insertContained things boxes
                Octree(newRoot, tree.BoundingBox.Union bounds, tree.LeafLimit, tree.GetBoundingBox)

            | Node(rootCell,rootBounds,cnt,childNodes) ->
                let newRootCell = Cell.GetCommonRoot(rootCell, c)

                let rec wrap (current : Cell) (targetCell : HashMap<Cell, OctNode<'a>>) =   
                    match HashMap.tryFind current targetCell with
                    | Some node ->
                        node
                    | None -> 
                        let children = current.Children
                        let currentCenter = current.GetCenter()

                        let a = Array.create 8 HashMap.empty
                        for (cell, node) in targetCell do   
                            let c = cell.GetCenter()
                            let o = 
                                (if c.Z > currentCenter.Z then 4 else 0) |||
                                (if c.Y > currentCenter.Y then 2 else 0) |||
                                (if c.X > currentCenter.X then 1 else 0) 
                            a.[o] <- HashMap.add cell node a.[o]

                        let newChildren = 
                            a |> Array.mapi (fun i map ->
                                if HashMap.isEmpty map then
                                    Empty
                                else
                                    let childCell = children.[i]
                                    wrap childCell map
                            )
                        Node(current, rootBounds, cnt, newChildren)

                let insertCells =
                    if rootCell.IsCenteredAtOrigin then
                        childNodes |> Array.choose (fun n ->
                            match n with
                            | Node(c,_,_,_) -> Some (c, n)
                            | Leaf(c,_,_,_) -> Some (c, n)
                            | Empty -> None
                        ) |> HashMap.ofArray
                    else
                        HashMap.single rootCell tree.Root

                let newRoot =
                    wrap newRootCell insertCells
                    |> insertContained things boxes
                Octree(newRoot, tree.BoundingBox.Union bounds, tree.LeafLimit, tree.GetBoundingBox)


    //let eps = 1E-6
    let inline private intersect (ray : FastRay3d, b : Box3d, t0 : byref<float>, t1 : byref<float>) =
        //let b = Box3d(b.Min - V3d(eps,eps,eps), b.Max + V3d(eps, eps, eps))
        ray.Intersects(b, &t0, &t1)

    let rayIntersections (ray : Ray3d) (tmin : float) (tmax : float) (tree : Octree<'a>) =
        let ray = FastRay3d ray
        let cmp = System.Func<_,_,_>(fun (t0, _) (t1,_) -> compare t0 t1)

        let rec traverse (ray : FastRay3d) (tmin : float) (tmax : float) (node : OctNode<'a>) =
            match node with
            | Empty ->
                Seq.empty
            | Leaf(_,b,_,es) ->
                let res = System.Collections.Generic.List()
                for e in es do
                    let bb = tree.GetBoundingBox e
                    let mutable t0 = tmin
                    let mutable t1 = tmax
                    if intersect(ray, bb, &t0, &t1) && t0 >= tmin && t0 <= tmax then
                        res.HeapEnqueue(cmp, (t0, e))
                    
                Array.init res.Count (fun _ -> res.HeapDequeue(cmp)) :> seq<_>

            | Node(_,b,_,cs) ->
                let mutable t0 = tmin
                let mutable t1 = tmax

                let subnodes = 
                    cs |> Array.choose (fun c ->
                        match c with
                        | Empty -> None
                        | Leaf(_,b,_,_) | Node(_,b,_,_) ->
                            t0 <- tmin
                            t1 <- tmax
                            if intersect(ray, b, &t0, &t1) && t1 >= tmin && t0 <= tmax then
                                let tmin = max t0 tmin
                                let tmax = min t1 tmax
                                Some (tmin, tmax, c)
                            else
                                None
                    )
                    |> Array.sortBy (fun (t,_,_) -> t)


                let overlap = 
                    let mutable o = false
                    let ranges = subnodes |> Array.map (fun (tmin, tmax,_) -> Range1d(tmin, tmax))
                    for i in 0 .. ranges.Length - 1 do
                        for j in i+1 .. ranges.Length - 1 do
                            let disjoint = ranges.[i].Max <= ranges.[j].Min || ranges.[i].Min >= ranges.[j].Max
                            if not disjoint then o <- true

                    o
                if overlap then
                    printfn "OVERLAPPING RANGES"
                    subnodes |> Seq.collect (fun (tmin, tmax, n) -> traverse ray tmin tmax n) |> Seq.sortBy fst
                else
                    subnodes |> Seq.collect (fun (tmin, tmax, n) -> traverse ray tmin tmax n)

        match tree.Root with
        | Empty -> Seq.empty
        | Leaf(_,b,_,_) | Node(_,b,_,_) ->
            let mutable t0 = tmin
            let mutable t1 = tmax
            if intersect(ray, b, &t0, &t1) && t1 >= tmin && t0 <= tmax then
                traverse ray (max tmin t0) (min tmax t1) tree.Root
            else
                Seq.empty


    let overlapping (box : Box3d) (tree : Octree<'a>) =
        let rec traverse (box : Box3d) (node : OctNode<'a>) =
            match node with
            | Empty -> 
                Seq.empty
            | Leaf(_,_,_,es) ->
                es |> Seq.filter (fun e ->
                    tree.GetBoundingBox(e).Intersects(box)
                )
            | Node(_,_,_,cs) ->
                cs |> Seq.collect (fun c ->
                    match c with
                    | Empty -> Seq.empty
                    | Leaf(_,b,_,_) | Node(_,b,_,_) ->
                        if b.Intersects box then
                            traverse box c
                        else
                            Seq.empty
                )

        traverse box tree.Root


