module Test.DirectedSuMGra

open Expecto
open FsCheck
open Swensen.Unquote

open DirectedSuMGra
open Graph
open MultiGraph

[<Tests>]
let tests =
    testList "DirectedSuMGra" [
        testCase "Finds expected mappings" <| fun () ->
            let target = set [
                ("t0", "class", "t0")
                ("t0", "contains", "t01")
                ("t01", "method", "t01")
                ("t0", "contains", "t02")
                ("t02", "method", "t02")
                ("t02", "public", "t02")
                ("t0", "contains", "t0a")
                ("t0a", "field", "t0a")
                ("t0a", "public", "t0a")
                ("t1", "class", "t1")
                ("t1", "contains", "t11")
                ("t11", "method", "t11")
                ("t1", "contains", "t1a")
                ("t1a", "field", "t1a")
                ("t1a", "private", "t1a")
                ("t2", "class", "t2")
                ("t2", "contains", "t21")
                ("t21", "public", "t21")
                ("t21", "method", "t21")
                ("t2", "contains", "t22")
                ("t22", "method", "t22")
                ("t22", "private", "t22")
            ]

            let query = set [
                ("p0", "class", "p0")
                ("p1", "method", "p1")
                ("p1", "public", "p1")
                ("p0", "contains", "p1")
            ]

            let expected = set [
                Map.ofList [ ("p0", "t0"); ("p1", "t02") ]
                Map.ofList [ ("p0", "t2"); ("p1", "t21") ]
            ]

            test <@ set (SubgraphSearch.searchSimpleGraph target query) = expected @>

        testProperty "All generated subgraphs have at least one mapping" <| fun (target: Graph<int, int>) ->
            Prop.forAll (Gen.subListOf target |> Gen.map set |> Arb.fromGen) <| fun (query: Graph<int, int>) ->
                not (Set.isEmpty target || Set.isEmpty query) ==>
                    lazy test <@ not (Seq.isEmpty (SubgraphSearch.searchSimpleGraph target query)) @>
    ]
