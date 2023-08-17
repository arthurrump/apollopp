module Test.RTree

open Expecto
open FsCheck
open Swensen.Unquote

open RTree
open System
open System.Collections.Immutable

module ImmArray =
    let inline contains (value: 't) (array: ImmutableArray<'t>) =
        array.Contains(value)

module Rect = 
    let genWithDimensions<'t when 't : comparison> n : Gen<Rect<'t>> =
        gen {
            let! arr2 = Gen.arrayOfLength (max n 1) Arb.generate<'t>
            let! arr1 = Gen.arrayOfLength (max n 1) Arb.generate<'t>
            return Rect.create (Array.min2 arr1 arr2) (Array.max2 arr1 arr2)
        }

type Entries<'k, 'v> = Entries of ('k[] * 'v)[]

type RTreeGen() =
    static member Rect<'t when 't : comparison>() = 
        gen {
            let! n = Arb.generate<int>
            return! Rect.genWithDimensions<'t> n
        } |> Arb.fromGen

    static member Entries<'k, 'v>() =
        gen {
            let! dimensions = Arb.generate<int> |> Gen.map abs |> Gen.filter (fun n -> n > 0)
            let pointGen = Gen.arrayOfLength dimensions Arb.generate<'k>
            let entryGen = Gen.zip pointGen Arb.generate<'v>
            let! entries = Gen.arrayOf entryGen |> Gen.filter (fun arr -> arr.Length > 0)
            return Entries entries
        } |> Arb.fromGen

let withGen = { FsCheckConfig.defaultConfig with arbitrary = [ typeof<RTreeGen> ] }
    
[<Tests>]
let tests =
    testList "RTree" [
        testList "Rect" [
            testCase "Create fails on zero-length vectors" <| fun () -> 
                raises<InvalidOperationException> <@ Rect.create [||] [||] @>
                raises<InvalidOperationException> <@ Rect.create [| 0 |] [||] @>
                raises<InvalidOperationException> <@ Rect.create [||] [| 0 |] @>

            testProperty "Create fails when low and high vectors have different lengths" <| 
                fun (arr1: int[]) (arr2: int[]) ->
                    arr1.Length <> arr2.Length ==>
                        lazy raises<InvalidOperationException> <@ Rect.create arr1 arr2 @>
            
            testProperty "Create fails when low vector is greater than high vector in any dimension" <| 
                fun (NonEmptyArray high) (index: int) ->
                    let index = (abs index) % high.Length
                    let low = high |> Array.updateAt index (high.[index] + 1 + index)
                    raises<InvalidOperationException> <@ Rect.create low high @>

            testProperty "Result of wrap should contain both inputs" <| fun n ->
                Prop.forAll (Arb.fromGen (Gen.two (Rect.genWithDimensions<int> n))) <| fun (rect1, rect2) ->
                    let wrapped = Rect.wrap rect1 rect2
                    test <@ Rect.contains wrapped rect1 && Rect.contains wrapped rect2 @>

            testPropertyWithConfig withGen "Contains itself" <| fun (rect: Rect<int>) ->
                test <@ Rect.contains rect rect @>
            
            testPropertyWithConfig withGen "Intersects itself" <| fun (rect: Rect<int>) ->
                test <@ Rect.intersects rect rect @>

            testPropertyWithConfig withGen "Created from origin contains origin" <| 
                fun ((NonEmptyArray point): NonEmptyArray<int>) ->
                    test <@ Rect.contains (Rect.createFromOrigin point) (Rect.createOrigin point.Length) @>
        ]
        testList "RTree" [
            testPropertyWithConfig withGen "Can find every entry when searched by point" <| 
                fun ((Entries entries): Entries<int, int>) ->
                    let tree = RTree.create 32 entries
                    for (point, value) in entries do
                        let result = RTree.search (Rect.createPoint point) tree
                        test <@ ImmArray.contains value result @>

            testCase "Can store and find multiple entries at the same point" <| fun () ->
                let tree = RTree.create 2 [|
                    [| 0; 1; 5; 4 |], 1
                    [| 1; 9; 8; 2 |], 2
                    [| 0; 1; 5; 4 |], 3
                    [| 1; 4; 0; 5 |], 4
                |]
                test <@ set (RTree.search (Rect.createPoint [| 0; 1; 5; 4 |]) tree) = set [ 1; 3 ] @>

            testCase "Finds entry when searched by Rect.fromOrigin" <| fun () ->
                let tree = RTree.create 2 [|
                    [| 0; 1; 5; 4 |], 1
                    [| 1; 9; 8; 2 |], 2
                    [| 0; 1; 5; 4 |], 3
                    [| 1; 4; 0; 5 |], 4
                |]
                test <@ set (RTree.search (Rect.createFromOrigin [| 1; 4; 6; 5 |]) tree) = set [ 1; 3; 4 ] @>

            testPropertyWithConfig withGen "Searching with wrapped finds all entries" <|
                fun ((Entries entries): Entries<int, int>) ->
                    let tree = RTree.create 32 entries
                    let wrapped = Rect.wrapAll (entries |> Array.map (fst >> Rect.createPoint))
                    let result = RTree.search wrapped tree
                    test <@ set result = set (entries |> Array.map snd) @>
        ]
    ]
