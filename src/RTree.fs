namespace RTree

// Simple RTree with bulk loading only, based on
// https://github.com/PanosCS/R-tree/blob/master/rtree.py
// https://gist.github.com/Horusiath/4bad956cd47ff3ba9b2fa27d164c176f

open System
open System.Collections.Immutable

module Array =
    let inline min2 a b = Array.map2 min a b
    let inline max2 a b = Array.map2 max a b
    let inline add a b = Array.map2 (fun a b -> a + b) a b
    let inline sub a b = Array.map2 (fun a b -> a - b) a b

    let inline eqAll a b = Array.forall2 (fun a b -> a = b) a b
    let inline neqAll a b = Array.forall2 (fun a b -> a <> b) a b
    let inline gtAll a b = Array.forall2 (fun a b -> a > b) a b
    let inline gteAll a b = Array.forall2 (fun a b -> a >= b) a b
    let inline ltAll a b = Array.forall2 (fun a b -> a < b) a b
    let inline lteAll a b = Array.forall2 (fun a b -> a <= b) a b

    let inline eqAny a b = Array.exists2 (fun a b -> a = b) a b
    let inline neqAny a b = Array.exists2 (fun a b -> a <> b) a b
    let inline gtAny a b = Array.exists2 (fun a b -> a > b) a b
    let inline gteAny a b = Array.exists2 (fun a b -> a >= b) a b
    let inline ltAny a b = Array.exists2 (fun a b -> a < b) a b
    let inline lteAny a b = Array.exists2 (fun a b -> a <= b) a b

[<NoComparison; NoEquality>] 
type Rect<'t> =
    { Low: 't[]
      High: 't[] }

module Rect =
    let create (low: 't[]) (high: 't[]) : Rect<'t> =
        if low.Length = 0 || high.Length = 0 then
            invalidOp "Low and high vectors must have at least one dimension"
        if low.Length <> high.Length then
            invalidOp "Low and high vectors must have the same length"
        if Array.gtAny low high then
            invalidOp "Low vector must be less than or equal to high vector in all dimensions"
        { Low = low; High = high }

    let createPoint (point: 't[]) : Rect<'t> =
        create point point

    let createOrigin (dimensions: int) : Rect<'t> =
        createPoint (Array.zeroCreate dimensions)

    let createFromOrigin (point: 't[]) : Rect<'t> =
        let zero = Array.zeroCreate point.Length
        create (Array.min2 zero point) (Array.max2 zero point)

    let dimensions (rect: Rect<'t>) : int =
        rect.Low.Length

    let wrap (rect1: Rect<'t>) (rect2: Rect<'t>) : Rect<'t> =
        { Low = Array.min2 rect1.Low rect2.Low
          High = Array.max2 rect1.High rect2.High }

    let wrapAll (rects: #seq<Rect<'t>>) : Rect<'t> = 
        Seq.reduce wrap rects

    let intersects (rect1: Rect<'t>) (rect2: Rect<'t>) : bool =
        not (Array.gtAll rect1.Low rect2.High || Array.ltAll rect1.High rect2.Low)

    let contains (rect1: Rect<'t>) (rect2: Rect<'t>) : bool =
        Array.lteAll rect1.Low rect2.Low && Array.gteAll rect1.High rect2.High

    let inline area (rect: Rect<'t>) : 't =
        Array.sub rect.High rect.Low |> Array.reduce (fun a b -> a * b)

type RTree<'k, 'v> =
    | Leaf of key: Rect<'k> * value: 'v
    | Branch of key: Rect<'k> * entries: ImmutableArray<RTree<'k, 'v>>

module RTree =
    let key : RTree<'k, 'v> -> Rect<'k> = function
        | Leaf(key, _) -> key
        | Branch(key, _) -> key

    let inline area (node: RTree<'k, 'v>) : 'k = 
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

    let create (maxCapacity: int) (entries: ('k[] * 'v)[]) : RTree<'k, 'v> =
        entries
        |> Array.map (fun (key, value) -> Leaf(Rect.createPoint key, value))
        |> createUpper maxCapacity

    let inline createSorted (maxCapacity: int) (entries: ('k[] * 'v)[]) : RTree<'k, 'v> =
        entries
        // Sort keys by "distance" from origin (works and might help if only positive coordinates)
        |> Array.sortBy (fun (key, _) -> Array.sum key)
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
        