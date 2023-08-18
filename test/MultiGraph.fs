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
                let multiGraph, nodeArray, edgeArray, _ = MultiGraph.fromGraph graph
                let result = MultiGraph.toGraph nodeArray edgeArray multiGraph
                result = graph
            @>

        testProperty "MultiGraph.[from, to'] = Graph.edgesFromTo" <| fun (graph: Graph<string, string>) ->
            let multigraph, nodeArray, edgeArray, _ = MultiGraph.fromGraph graph
            for from in 0 .. MultiGraph.nodeCount multigraph - 1 do
                for to' in 0 .. MultiGraph.nodeCount multigraph - 1 do
                    let mgEdges = multigraph.[from, to'] |> Set.map (fun edge -> edgeArray.[edge])
                    let graphEdges = Graph.edgesFromTo nodeArray.[from] nodeArray.[to'] graph
                    test <@ mgEdges = graphEdges @>
    ]
