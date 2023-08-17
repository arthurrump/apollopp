namespace RTree

// Simple RTree with bulk loading only, based on
// https://github.com/PanosCS/R-tree/blob/master/rtree.py
// https://gist.github.com/Horusiath/4bad956cd47ff3ba9b2fa27d164c176f

open System
open System.Collections.Immutable
open System.Numerics

[<Struct; NoComparison; NoEquality>] 
type Rect<'t when 't: struct and 't :> ValueType and 't: (new: unit -> 't)> =
    { Low: Vector<'t>
      High: Vector<'t> }

module Rect =
    let create (low: Vector<'t>) (high: Vector<'t>) : Rect<'t> =
        { Low = low; High = high }

    let createPoint (point: Vector<'t>) : Rect<'t> =
        { Low = point; High = point }

    let wrap (rect1: Rect<'t>) (rect2: Rect<'t>) : Rect<'t> =
        { Low = Vector.Min(rect1.Low, rect2.Low)
          High = Vector.Max(rect1.High, rect2.High)}

    let wrapAll (rects: #seq<Rect<'t>>) : Rect<'t> = 
        Seq.reduce wrap rects

    let intersects (rect1: Rect<'t>) (rect2: Rect<'t>) : bool =
        not (Vector.GreaterThanAll(rect1.Low, rect2.High) || Vector.LessThanAll(rect1.High, rect2.Low))

    let contains (rect1: Rect<'t>) (rect2: Rect<'t>) : bool =
        Vector.LessThanOrEqualAll(rect1.Low, rect2.Low) && Vector.GreaterThanOrEqualAll(rect1.High, rect2.High)

    let area (rect: Rect<'t>) : 't =
        Vector.Dot(rect.High - rect.Low, Vector.One)

type RTree<'k, 'v when 'k: struct and 'k :> ValueType and 'k: (new: unit -> 'k)> =
    | Leaf of key: Rect<'k> * value: 'v
    | Branch of key: Rect<'k> * entries: ImmutableArray<RTree<'k, 'v>>

module RTree =
    let key : RTree<'k, 'v> -> Rect<'k> = function
        | Leaf(key, _) -> key
        | Branch(key, _) -> key

    let area (node: RTree<'k, 'v>) : 'k = 
        key node |> Rect.area

    let private createBranch (nodes: RTree<'k, 'v>[]) : RTree<'k, 'v> =
        let key = nodes |> Array.map key |> Rect.wrapAll
        Branch (key, nodes |> ImmutableArray.ToImmutableArray)

    let rec private createUpper (maxCapacity: int) (nodes: RTree<'k, 'v>[]) : RTree<'k, 'v> =
        if nodes |> Array.length < maxCapacity then
           createBranch nodes
        else
            nodes
            |> Array.chunkBySize maxCapacity
            |> Array.map createBranch
            |> createUpper maxCapacity

    let create (maxCapacity: int) (entries: (Vector<'k> * 'v)[]) : RTree<'k, 'v> =
        entries
        |> Array.map (fun (key, value) -> Leaf(Rect.createPoint key, value))
        |> createUpper maxCapacity

    let createSorted (maxCapacity: int) (entries: (Vector<'k> * 'v)[]) : RTree<'k, 'v> =
        entries
        // Sort keys by "distance" from origin (works if only positive coordinates)
        |> Array.sortBy (fun (key, _) -> Vector.Dot(key, Vector.One))
        |> create maxCapacity

    let search (rect: Rect<'k>) (node: RTree<'k, 'v>) : ImmutableArray<'v> =
        let rec loop (results: ResizeArray<'v>) (rect: Rect<'k>) = function
            | Leaf(key, value) when Rect.contains rect key ->
                results.Add(value)
            | Branch(key, entries) when Rect.intersects rect key ->
                for entry in entries do
                    loop results rect entry
            | _ -> 
                ()
        let results = ResizeArray()
        loop results rect node
        ImmutableArray.ToImmutableArray results
        