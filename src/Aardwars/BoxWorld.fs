namespace Aardwars

open Aardvark.Base
open Aardvark.Base.Sorting
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.SceneGraph
open Microsoft.FSharp.NativeInterop

type V3s =
    struct
        val mutable public X : int16
        val mutable public Y : int16
        val mutable public Z : int16

        new(x : int16, y : int16, z : int16) = { X = x; Y = y; Z = z }
        new(x : int, y : int, z : int) = { X = int16 x; Y = int16 y; Z = int16 z }

        static member (+) (a : V3s, b : V3s) = V3s(a.X + b.X, a.Y + b.Y, a.Z + b.Z)
        static member (*) (a : V3s, b : int16) = V3s(a.X * b, a.Y * b, a.Z * b)

    end

[<Struct>]
type BoxInfo =
    {
        Offset : V3s
        MaterialId : V3s
    }

    member x.BoundingBox =
        let o = V3d(float x.Offset.X, float x.Offset.Y, float x.Offset.Z)
        Box3d(o, o + V3d.III)



type OctNode<'a> =
    | Empty
    | Leaf of cell : Cell * count : int * elements : list<'a>
    | Node of cell : Cell * count : int * OctNode<'a>[]
    

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
        let g = IndexedGeometryPrimitives.Box.solidBox Box3d.Unit C4b.White
        g.Flat
    
    let children = 
        lazy (
            let realRoot =
                match root with
                | Some r -> r
                | None -> this
            match node with
            | Node(_,_,e) -> 
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
        | Leaf(c,_,_) -> true
        | _ -> false

    let localCellBounds =
        match node with
        | Empty -> Box3d.Unit
        | Leaf(c,_,_) -> c.BoundingBox
        | Node(c,_,_) -> c.BoundingBox

    let equivalentAngle60 (view : Trafo3d) (proj : Trafo3d) =

        let cam = view.Backward.C3.XYZ

        let avgPointDistance = localCellBounds.Size.NormMax / 40.0

        let minDist = localCellBounds.GetMinimalDistanceTo(cam)
        let minDist = max 0.01 minDist

        let angle = Constant.DegreesPerRadian * atan2 avgPointDistance minDist

        let fov = fov proj

        60.0 * angle / fov 

    interface ILodTreeNode with
        member this.Acquire() = ()
        member this.Cell =
            match node with
            | Empty -> Cell.Unit
            | Leaf(c,_,_) -> c
            | Node(c,_,_) -> c

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
            | Leaf(_,_,es) ->
                let offsets = 
                    es |> List.map (fun e -> 
                        let pa = fixed [| (uint32 e.MaterialId.Y <<< 16) ||| (uint32 e.MaterialId.X) |]
                        let f : float32 = NativePtr.read (NativePtr.cast pa)
                        V4f(float32 e.Offset.X, float32 e.Offset.Y, float32 e.Offset.Z, f)
                    ) |> List.toArray
                let scales =    
                    es |> List.map (fun e -> 
                        V4f(1.0f, 1.0f, 1.0f, float32 e.MaterialId.Z)
                    ) |> List.toArray

                geometry, 
                MapExt.ofList [
                    "Offset", offsets :> System.Array
                    "Scale", scales :> System.Array
                ]
            | Node(c,_,_) ->
                let bb = c.BoundingBox
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

        member this.DataSize =
            match node with
            | Empty -> 0
            | Leaf(_,c,_) -> c
            | Node _ -> 1

        member this.DataSource = DefaultSemantic.CreaseAngle
        member this.DataTrafo = Trafo3d.Identity
        member this.Id = this :> obj
        member this.Level = level
        member this.Name = "Hans"
        member this.Release() = ()
        member this.TotalDataSize = 
            match node with
            | Empty -> 0
            | Leaf(_,c,_) -> c
            | Node(_,c,_) -> c
        member this.WorldBoundingBox = 
            match node with
            | Empty -> Box3d.Invalid
            | Leaf(c,_,_) -> c.BoundingBox
            | Node(c,_,_) -> c.BoundingBox
        member this.WorldCellBoundingBox = 
            match node with
            | Empty -> Box3d.Invalid
            | Leaf(c,_,_) -> c.BoundingBox
            | Node(c,_,_) -> c.BoundingBox

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
        | Leaf(_,c,_) -> c
        | Node(_,c,_) -> c

    member x.GetIntersecting(b : Box3d) =
        let rec findIntersecting (b : Box3d) (node : OctNode<'a>) =
            match node with
            | Empty -> Seq.empty
            | Leaf(cell,_,e) ->
                if cell.BoundingBox.Intersects b then
                    e |> Seq.filter (fun e -> (getBoundingBox e).Intersects b)
                else 
                    Seq.empty
            | Node(cell,_,c) ->
                if cell.BoundingBox.Intersects b then
                    c |> Seq.collect (findIntersecting b)
                else
                    Seq.empty

        findIntersecting b root




module Octree =
    let empty<'a> (limit : int) (getBoundingBox : 'a -> Box3d) = Octree<'a>(Empty, Box3d.Invalid, limit, getBoundingBox)

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
                | Leaf(cell, cnt, elements) ->
                        
                    let newCnt = cnt + boxes.Length
                    if newCnt <= tree.LeafLimit then
                        Leaf(cell, newCnt, Array.toList things @ elements)
                    else
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

                        for thing in elements do
                            let box = tree.GetBoundingBox thing
                            let cc = box.Center

                            let oct = // TODO: lookup cell impl
                                (if cc.Z > c.Z then 4 else 0) |||
                                (if cc.Y > c.Y then 2 else 0) |||
                                (if cc.X > c.X then 1 else 0) 

                            subThings.[oct].Add thing
                            subBoxes.[oct].Add box
                            


                        let childCells = cell.Children
                        let children =
                            Array.init 8 (fun i ->
                                let things = subThings.[i].ToArray()
                                let boxes = subBoxes.[i].ToArray()
                                if things.Length > 0 then
                                    Leaf(childCells.[i], 0, []) |> insertContained things boxes 
                                else
                                    Empty
                            )
                        Node(cell, cnt + boxes.Length, children)

                | Node(cell, cnt, children) ->
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
                                | Empty -> Leaf(cell.Children.[i], 0, []) |> insertContained things boxes
                                | c -> insertContained things boxes c
                            else c
                        )   
                    Node(cell, cnt + boxes.Length, newChildren)

            match tree.Root with
            | Empty -> 
                let newRoot = 
                    Leaf(c, 0, [])
                    |> insertContained things boxes 
                Octree(newRoot, tree.BoundingBox.Union bounds, tree.LeafLimit, tree.GetBoundingBox)

            | Leaf(rootCell, cnt, elements) ->
                let newRootCell = Cell.GetCommonRoot(rootCell, c)
                let newRoot =
                    Leaf(newRootCell, cnt, elements)
                    |> insertContained things boxes
                Octree(newRoot, tree.BoundingBox.Union bounds, tree.LeafLimit, tree.GetBoundingBox)

            | Node(rootCell,cnt,childNodes) ->
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
                        Node(current, cnt, newChildren)

                let insertCells =
                    if rootCell.IsCenteredAtOrigin then
                        childNodes |> Array.choose (fun n ->
                            match n with
                            | Node(c,_,_) -> Some (c, n)
                            | Leaf(c,_,_) -> Some (c, n)
                            | Empty -> None
                        ) |> HashMap.ofArray
                    else
                        HashMap.single rootCell tree.Root

                let newRoot =
                    wrap newRootCell insertCells
                    |> insertContained things boxes
                Octree(newRoot, tree.BoundingBox.Union bounds, tree.LeafLimit, tree.GetBoundingBox)





