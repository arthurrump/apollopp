namespace RTree

// Simple RTree with bulk loading only, based on
// https://github.com/PanosCS/R-tree/blob/master/rtree.py
// https://gist.github.com/Horusiath/4bad956cd47ff3ba9b2fa27d164c176f

open System
open System.Collections.Immutable

module Array =
    open Microsoft.FSharp.Core.LanguagePrimitives

    let inline min2 a b = Array.map2 min a b
    let inline max2 a b = Array.map2 max a b
    let inline avg2 a b = Array.map2 (fun a b -> (a + b) / (GenericOne + GenericOne)) a b
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

    let point (point: 't[]) : Rect<'t> =
        create point point

    let origin (dimensions: int) : Rect<'t> =
        point (Array.zeroCreate dimensions)

    let fromOriginToPoint (point: 't[]) : Rect<'t> =
        let zero = Array.zeroCreate point.Length
        create (Array.min2 zero point) (Array.max2 zero point)

    let dimensions (rect: Rect<'t>) : int =
        rect.Low.Length

    let inline center (rect: Rect<'t>) : 't[] =
        Array.avg2 rect.Low rect.High

    let wrap (rect1: Rect<'t>) (rect2: Rect<'t>) : Rect<'t> =
        { Low = Array.min2 rect1.Low rect2.Low
          High = Array.max2 rect1.High rect2.High }

    let wrapAll (rects: #seq<Rect<'t>>) : Rect<'t> = 
        Seq.reduce wrap rects

    let intersection (rect1: Rect<'t>) (rect2: Rect<'t>) : Rect<'t> =
        { Low = Array.max2 rect1.Low rect2.Low
          High = Array.min2 rect1.High rect2.High }

    let intersectionAll (rects: #seq<Rect<'t>>) : Rect<'t> =
        Seq.reduce intersection rects

    let intersects (rect1: Rect<'t>) (rect2: Rect<'t>) : bool =
        not (Array.gtAll rect1.Low rect2.High || Array.ltAll rect1.High rect2.Low)

    /// <summary>
    /// Evaluates to true if the area of the first rectangle is fully contained
    /// in the second.
    /// </summary>
    /// <returns>True if <c>rect2</c> contains <c>rect1</c>.</returns>
    let contains (rect1: Rect<'t>) (rect2: Rect<'t>) : bool =
        Array.lteAll rect2.Low rect1.Low && Array.gteAll rect2.High rect1.High

    let inline area (rect: Rect<'t>) : 't =
        Array.sub rect.High rect.Low |> Array.reduce (fun a b -> a * b)

type RTree<'k, 'v> =
    | Leaf of key: Rect<'k> * value: 'v
    | Branch of key: Rect<'k> * entries: ImmutableArray<RTree<'k, 'v>>

module RTree =
    open System.Collections.Generic
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
    
    let createInOrder (maxCapacity: int) (entries: (Rect<'k> * 'v)[]) : RTree<'k, 'v> =
        entries
        |> Array.map Leaf
        |> createUpper maxCapacity

    let createFromPointsInOrder (maxCapacity: int) (entries: ('k[] * 'v)[]) : RTree<'k, 'v> =
        entries
        |> Array.map (fun (key, value) -> Rect.point key, value)
        |> createInOrder maxCapacity

    type private STREntryComparer<'k, 'v when 'k : comparison>(dimension: int) =
        interface IComparer<'k[] * Rect<'k> * 'v> with
            member __.Compare((key1, _, _), (key2, _, _)) =
                compare key1.[dimension] key2.[dimension]

    let createSorted (maxCapacity: int) (entries: (Rect<'k> * 'v)[]) =
        // Based on https://doi.org/10.1109/ICDE.1997.582015 
        // Where each group of b is intended to be placed in the same leaf level
        // node. (...) Consider a k-dimensional data set of r hyper-rectangles.
        // (...) We assume coordinates are for the center points of the
        // rectangles. [The case k > 2 is a simple generalization of the
        // approach described above.]
        let rec sort dimensions currentDimension (entries: Memory<'k[] * Rect<'k> *'v>) =
            // Determine the number of leaf level pages P = ceil(r / b) and
            let leafCount = ceil (float entries.Length / float maxCapacity)
            // [Sort the hyperrectangles according to the first coordinate of
            // their center.]
            entries.Span.Sort(STREntryComparer(currentDimension))
            // [Divide the input into S = ceil(P^(1/k)) slabs, where a slab
            // consists of a run of b*ceil(P^((k-1)/k)) consecutive
            // hyper-rectangles.]
            let slabSize = maxCapacity * int (ceil (Math.Pow(leafCount, float (dimensions - 1) / float dimensions)))
            if slabSize > 1 then
                for i in 0 .. slabSize .. entries.Length - 1 do
                    // Note that the last [slab] may contain fewer than [...]
                    // rectangles.
                    let slab = entries.Slice(i, min slabSize (entries.Length - i))
                    // Each slab is now processed recursively, using the remaining k
                    // - 1 coordinates.
                    if currentDimension < dimensions - 1 then
                        sort dimensions (currentDimension + 1) slab
        
        let entries = entries |> Array.map (fun (rect, value) -> Rect.center rect, rect, value)
        let dimensions = 
            let (center, _, _) = entries.[0]
            center.Length
        do sort dimensions 0 (Memory entries)
        
        entries
        |> Array.map (fun (_, rect, value) -> rect, value)
        |> createInOrder maxCapacity

    let createFromPointsSorted (maxCapacity: int) (entries: ('k[] * 'v)[]) : RTree<'k, 'v> =
        entries
        |> Array.map (fun (key, value) -> Rect.point key, value)
        |> createSorted maxCapacity

    let private search leafCondition branchCondition region node =
        let rec loop (results: ResizeArray<'v>) (rect: Rect<'k>) = function
            | Leaf (key, value) when leafCondition key rect ->
                results.Add(value)
            | Branch (key, entries) when branchCondition key rect ->
                for entry in entries do
                    loop results rect entry
            | _ ->
                ()
        let results = ResizeArray()
        loop results region node
        ImmutableArray.ToImmutableArray results

    /// Get all values inside the given rectangle
    let searchInside (rect: Rect<'k>) (node: RTree<'k, 'v>) : ImmutableArray<'v> =
        search (fun key rect -> Rect.contains key rect) (fun key rect -> Rect.intersects key rect) rect node
    
    /// Get all values that contain the given rectangle
    let searchContainment (rect: Rect<'k>) (node: RTree<'k, 'v>) : ImmutableArray<'v> =
        search (fun key rect -> Rect.contains rect key) (fun key rect -> Rect.contains rect key) rect node
    
    /// Get all values that intersect the given rectangle
    let searchIntersection (rect: Rect<'k>) (node: RTree<'k, 'v>) : ImmutableArray<'v> =
        search (fun key rect -> Rect.intersects key rect) (fun key rect -> Rect.intersects key rect) rect node
