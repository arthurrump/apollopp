module Test.MultiGraph

open Expecto
open Swensen.Unquote

open Graph
open MultiGraph

[<Tests>]
let tests =
    testList "MultiGraph" [
        testProperty "fromGraph >> toGraph yields original graph" <| fun (graph: Graph<string, string>) ->
            test <@ 
                let multiGraph, nodeArray = MultiGraph.fromGraph graph
                let result = MultiGraph.toGraph nodeArray multiGraph
                result = graph
            @>

        testProperty "MultiGraph.[from, to'] = Graph.edgesFromTo" <| fun (graph: Graph<string, string>) ->
            let multigraph, nodeArray = MultiGraph.fromGraph graph
            for from in 0 .. MultiGraph.nodeCount multigraph - 1 do
                for to' in 0 .. MultiGraph.nodeCount multigraph - 1 do
                    test <@ 
                        multigraph.[from, to'] = 
                            Graph.edgesFromTo (Seq.item from nodeArray) (Seq.item to' nodeArray) graph 
                    @>
    ]
