module Test.MultiGraph

open Expecto
open Swensen.Unquote

open Graph
open MultiGraph

module ImmArray =
    open System.Collections.Immutable
    let inline length (array: ImmutableArray<'t>) = 
        array.Length
    let inline slice (start: int, length: int) (array: ImmutableArray<'t>) =
        array.Slice(start, length)

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

        testProperty "extendWithGraph leaves original node indices intact" <| 
            fun (graph: Graph<string, string>, extension: Graph<string, string>) ->
                let multigraph, originalNodeArray = 
                    MultiGraph.fromGraph graph
                let _, extendedNodeArray = 
                    MultiGraph.extendWithGraph extension (multigraph, originalNodeArray)
                test <@ ImmArray.slice (0, ImmArray.length originalNodeArray) extendedNodeArray = originalNodeArray @>

        testProperty "extendWithGraph creates a superset of the original" <|
            fun (graph: Graph<string, string>, extension: Graph<string, string>) ->
                let multigraph, nodeArray = MultiGraph.fromGraph graph
                let extended, _ = MultiGraph.extendWithGraph extension (multigraph, nodeArray)
                for from in 0 .. MultiGraph.nodeCount multigraph - 1 do
                    for to' in 0 .. MultiGraph.nodeCount multigraph - 1 do
                        test <@ Set.isSuperset extended.[from, to'] multigraph.[from, to'] @>

        testProperty "extendWithGraph.[from, to'] is a superset of Graph.edgesFromTo extension" <|
            fun (graph: Graph<int, int>, extension: Graph<int, int>) ->
                let multigraph, nodeArray = MultiGraph.fromGraph graph
                let extended, extendedNodeArray = MultiGraph.extendWithGraph extension (multigraph, nodeArray)
                for from in 0 .. MultiGraph.nodeCount extended - 1 do
                    for to' in 0 .. MultiGraph.nodeCount extended - 1 do
                        test <@ 
                            Set.isSuperset 
                                extended.[from, to'] 
                                (Graph.edgesFromTo (Seq.item from extendedNodeArray) (Seq.item to' extendedNodeArray) extension) 
                        @>
    ]
