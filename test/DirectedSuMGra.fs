module Test.DirectedSuMGra

open Expecto
open FsCheck
open Swensen.Unquote

open DirectedSuMGra
open Graph
open MultiGraph

let arbitraryMapping querySize targetSize =
    gen {
        let! shuffledTargets = Gen.shuffle [ 0 .. targetSize - 1 ]
        return 
            shuffledTargets
            |> Seq.take querySize
            |> Seq.indexed
            |> Map.ofSeq
    } |> Arb.fromGen

[<Tests>]
let tests =
    testList "DirectedSuMGra" [
        testList "Query" [
            testProperty "extendWithGraph preserves original order" <| 
                fun (graph: Graph<string, string>, extension: Graph<string, string>) ->
                    not (Set.isEmpty graph) ==> lazy
                        let mg, nodeArray = MultiGraph.fromGraph graph
                        let query = Query.fromGraph mg
                        let extended, _ = MultiGraph.extendWithGraph extension (mg, nodeArray)
                        let extendedQuery = Query.extendWithGraph extended query
                        test <@ List.take (List.length query.Order) extendedQuery.Order = query.Order @>
        ]
        testList "SubgraphSearch" [
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
                    not (Set.isEmpty target || Set.isEmpty query) ==> lazy
                        test <@ not (Seq.isEmpty (SubgraphSearch.searchSimpleGraph target query)) @>

            testProperty "All mappings can be verified" <| fun (target: Graph<int, int>) (query: Graph<int, int>) ->
                not (Set.isEmpty target || Set.isEmpty query) ==> lazy
                    let targetMg = MultiGraph.fromGraph target |> fst
                    let queryMg = MultiGraph.fromGraph query |> fst
                    test <@ SubgraphSearch.searchSimple targetMg queryMg |> Seq.forall (SubgraphSearch.verify targetMg queryMg) @>

            testProperty "All mappings that are not returned, cannot be verified" <| fun (target: Graph<int, int>) (query: Graph<int, int>) ->
                not (Set.isEmpty target || Set.isEmpty query) ==> lazy
                    let targetMg, targetNodes = MultiGraph.fromGraph target
                    let queryMg, queryNodes = MultiGraph.fromGraph query
                    queryNodes.Length <= targetNodes.Length ==> lazy
                        let mappings = set (SubgraphSearch.searchSimple targetMg queryMg)
                        Prop.forAll (arbitraryMapping queryNodes.Length targetNodes.Length) <| fun mapping ->
                            test <@ SubgraphSearch.verify targetMg queryMg mapping = Seq.contains mapping mappings @>

            testProperty "search base |> searchExtended ext = search ext, both trough Query.extendWithGraph" <|
                fun (target: Graph<int, int>, queryBase: Graph<int, int>, queryExtension: Graph<int, int>) ->
                    not (Set.isEmpty target || Set.isEmpty queryBase) ==> lazy
                        let target = Target.fromGraph (MultiGraph.fromGraph target |> fst)
                        let queryBaseMg, queryBaseNodeArray = MultiGraph.fromGraph queryBase
                        let queryBase = Query.fromGraph queryBaseMg
                        let queryExtMg, _ = MultiGraph.extendWithGraph queryExtension (queryBaseMg, queryBaseNodeArray)
                        let queryExt = Query.extendWithGraph queryExtMg queryBase

                        let resultsDirect = SubgraphSearch.search target queryExt
                        let resultsExtended = 
                            SubgraphSearch.search target queryBase 
                            |> Seq.collect (SubgraphSearch.searchExtended target queryExt)

                        test <@ set resultsExtended = set resultsDirect @>

            testProperty "search base |> searchExtended ext = search ext', with ext' through Query.fromGraph" <|
                fun (target: Graph<int, int>, queryBase: Graph<int, int>, queryExtension: Graph<int, int>) ->
                    not (Set.isEmpty target || Set.isEmpty queryBase) ==> lazy
                        let target = Target.fromGraph (MultiGraph.fromGraph target |> fst)
                        let queryBaseMg, queryBaseNodeArray = MultiGraph.fromGraph queryBase
                        let queryBase = Query.fromGraph queryBaseMg
                        let queryExtMg, _ = MultiGraph.extendWithGraph queryExtension (queryBaseMg, queryBaseNodeArray)
                        let queryExt = Query.extendWithGraph queryExtMg queryBase
                        let queryExt' = Query.fromGraph queryExtMg

                        let resultsDirect = SubgraphSearch.search target queryExt'
                        let resultsExtended = 
                            SubgraphSearch.search target queryBase 
                            |> Seq.collect (SubgraphSearch.searchExtended target queryExt)
                        
                        test <@ set resultsExtended = set resultsDirect @>

            testProperty "Extending an empty map gives the same results as a normal search" <| fun (target: Graph<int, int>) (query: Graph<int, int>) ->
                not (Set.isEmpty target || Set.isEmpty query) ==> lazy
                    let target = Target.fromGraph (MultiGraph.fromGraph target |> fst)
                    let query = Query.fromGraph (MultiGraph.fromGraph query |> fst)
                    test <@ set (SubgraphSearch.search target query) = set (SubgraphSearch.searchExtended target query Map.empty) @>
        ]
    ]
