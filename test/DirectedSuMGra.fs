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
                        let query = Query.fromGraph "testRoot" graph
                        let extendedQuery = Query.extendWithGraph "testExtension" extension query
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
                    let target = Target.fromGraph "target" target
                    let query = Query.fromGraph "query" query
                    test <@ SubgraphSearch.search target query |> Seq.forall (SubgraphSearch.verify target.Graph query.Graph) @>

            testProperty "All mappings that are not returned, cannot be verified" <| fun (target: Graph<int, int>) (query: Graph<int, int>) ->
                not (Set.isEmpty target || Set.isEmpty query) ==> lazy
                    let target = Target.fromGraph "target" target
                    let query = Query.fromGraph "query" query
                    query.NodeArray.Length <= target.NodeArray.Length ==> lazy
                        let mappings = set (SubgraphSearch.search target query)
                        Prop.forAll (arbitraryMapping query.NodeArray.Length target.NodeArray.Length) <| fun mapping ->
                            test <@ SubgraphSearch.verify target.Graph query.Graph mapping = Seq.contains mapping mappings @>

            testProperty "search base |> searchExtended ext = search ext, both trough Query.extendWithGraph" <|
                fun (target: Graph<int, int>, queryBase: Graph<int, int>, queryExtension: Graph<int, int>) ->
                    not (Set.isEmpty target || Set.isEmpty queryBase) ==> lazy
                        let target = Target.fromGraph "target" target
                        let queryBase = Query.fromGraph "queryBase" queryBase
                        let queryExt = Query.extendWithGraph "queryExt" queryExtension queryBase

                        let resultsDirect = SubgraphSearch.search target queryExt
                        let resultsExtended = 
                            SubgraphSearch.search target queryBase 
                            |> Seq.collect (SubgraphSearch.searchExtended target queryExt)

                        test <@ set resultsExtended = set resultsDirect @>

            testProperty "search base |> searchExtended ext = search ext', with ext' through Query.fromGraph" <|
                fun (targetGraph: Graph<int, int>, queryBaseGraph: Graph<int, int>, queryExtension: Graph<int, int>) ->
                    not (Set.isEmpty targetGraph || Set.isEmpty queryBaseGraph) ==> lazy
                        let target = Target.fromGraph "target" targetGraph
                        let queryBase = Query.fromGraph "queryBase" queryBaseGraph
                        let queryExt = Query.extendWithGraph "queryExt" queryExtension queryBase
                        let queryExt' = Query.fromGraph "queryExt'" (Graph.combine queryBaseGraph queryExtension)

                        let resultsDirect = SubgraphSearch.search target queryExt'
                        let resultsExtended = 
                            SubgraphSearch.search target queryBase 
                            |> Seq.collect (SubgraphSearch.searchExtended target queryExt)
                        
                        test <@ set resultsExtended = set resultsDirect @>

            testCase "search base |> searchExtended ext = search ext' with disconnected query" <| fun () ->
                let targetGraph = set [ 0, 0, 0; 0, 2, 1 ]
                let queryBaseGraph = set [ 0, 0, 0 ]
                let queryExtension = set [ 2, 2, 1 ]
                let target = Target.fromGraph "target" targetGraph
                let queryBase = Query.fromGraph "queryBase" queryBaseGraph
                let queryExt = Query.extendWithGraph "queryExt" queryExtension queryBase
                let queryExt' = Query.fromGraph "queryExt'" (Graph.combine queryBaseGraph queryExtension)

                let resultsDirect = SubgraphSearch.search target queryExt'
                let resultsExtended = 
                    SubgraphSearch.search target queryBase 
                    |> Seq.collect (SubgraphSearch.searchExtended target queryExt)
                
                test <@ set resultsExtended = set resultsDirect @>

            testProperty "Extending an empty map gives the same results as a normal search" <| fun (target: Graph<int, int>) (query: Graph<int, int>) ->
                not (Set.isEmpty target || Set.isEmpty query) ==> lazy
                    let target = Target.fromGraph "target" target
                    let query = Query.fromGraph "query" query
                    test <@ set (SubgraphSearch.search target query) = set (SubgraphSearch.searchExtended target query Map.empty) @>
        ]
    ]
