namespace MultiGraph

open Graph
open System.Collections.Immutable
open Thoth.Json.Net

type MultiGraph<'edge when 'edge : comparison> = Set<'edge>[,]
type Signature<'edge when 'edge : comparison> =
    { Incoming: ImmutableArray<Set<'edge>>
      Outgoing: ImmutableArray<Set<'edge>>
      Loops: Set<'edge> }

module MultiGraph =
    let initEmpty (nodeCount: int) : MultiGraph<'edge> = 
        Array2D.create nodeCount nodeCount Set.empty

    let fromIntGraph (graph: Graph<int, 'edge>) : MultiGraph<'edge> =
        let nodeCount = graph |> Graph.nodes |> Set.count
        let mg = initEmpty nodeCount
        for from, edge, to' in graph do
            mg.[from, to'] <- Set.add edge mg.[from, to']
        mg

    let toIntGraphSeq (graph: MultiGraph<'edge>) : seq<Edge<int, 'edge>> =
        seq {
            for from in 0 .. Array2D.length1 graph - 1 do
                for to' in 0 .. Array2D.length1 graph - 1 do
                    for edge in graph.[from, to'] do
                        yield from, edge, to'
        }

    let toIntGraph (graph: MultiGraph<'edge>) : Graph<int, 'edge> =
        set (toIntGraphSeq graph)
    
    let fromGraph (graph: Graph<'node, 'edge>) : MultiGraph<'edge> * ImmutableArray<'node> =
        let nodeArray = graph |> Graph.nodes |> ImmutableArray.ToImmutableArray
        let nodeMap = nodeArray |> Seq.indexed |> Seq.map (fun (i, node) -> node, i) |> Map.ofSeq
        let mg = initEmpty nodeArray.Length
        for from, edge, to' in graph do
            mg[nodeMap[from], nodeMap[to']] <- Set.add edge mg.[nodeMap[from], nodeMap[to']]
        mg, nodeArray

    let toGraph (nodeArray: ImmutableArray<'node>) (graph: MultiGraph<'edge>) : Graph<'node, 'edge> =
        set <| seq {
            for from in 0 .. Array2D.length1 graph - 1 do
                for to' in 0 .. Array2D.length1 graph - 1 do
                    for edge in graph.[from, to'] do
                        yield nodeArray.[from], edge, nodeArray.[to']
        }

    let extendWithGraph (extension: Graph<'node, 'edge>) (graph: MultiGraph<'edge>, nodeArray: ImmutableArray<'node>) : MultiGraph<'edge> * ImmutableArray<'node> =
        let originalNodeCount = nodeArray.Length
        let newNodes = Set.difference (Graph.nodes extension) (set nodeArray)
        let nodeArray = nodeArray.AddRange newNodes
        let graph =
            Array2D.init nodeArray.Length nodeArray.Length (fun from to' ->
                let existing =
                    if from < originalNodeCount && to' < originalNodeCount 
                    then graph.[from, to']
                    else Set.empty
                Set.union existing (Graph.edgesFromTo nodeArray.[from] nodeArray.[to'] extension)
            )
        graph, nodeArray

    let nodeCount : MultiGraph<'edge> -> int = Array2D.length1

    let getMultiEdge (from: int) (to': int) (graph: MultiGraph<'edge>) : Set<'edge> =
        graph.[from, to']

    let adjacent (node: int) (graph: MultiGraph<'edge>) =
        let filterConnections =
            Array.mapi (fun to' edges -> to', edges) 
            >> Array.choose (fun (to', edges) -> if Set.isEmpty edges || to' = node then None else Some to')
        Set.union 
            (set (filterConnections graph.[node, *]))
            (set (filterConnections graph.[*, node]))

    /// Returns all nodes that are adjacent to one of <c>nodes</c>, or the empty
    /// set if <c>nodes</c> is empty
    let allAdjacent (nodes: #seq<int>) (graph: MultiGraph<'edge>) =
        nodes 
        |> Seq.map (fun node -> adjacent node graph) 
        |> Seq.fold Set.union Set.empty

    let inducedGraph (nodes: #seq<int>) (graph: MultiGraph<'edge>) : Graph<int, 'edge> =
        set <| seq {
            for (from, to') in Seq.allPairs nodes nodes do
                for edge in graph.[from, to'] do
                    yield from, edge, to'
        }

    let signature (graph: MultiGraph<'edge>) (node: int) : Signature<'edge> =
        let filterEdges edges =
            edges
            |> Seq.indexed
            |> Seq.filter (fun (i, edges) -> not (Set.isEmpty edges || i = node))
            |> Seq.map snd
            |> ImmutableArray.ToImmutableArray
        { Incoming = filterEdges graph.[*, node]
          Outgoing = filterEdges graph.[node, *]
          Loops = graph.[node, node] }

    let signatureMap (graph: MultiGraph<'edge>) : ImmutableArray<Signature<'edge>> =
        Seq.init (nodeCount graph) (signature graph)
        |> ImmutableArray.ToImmutableArray

    let encode (encoder: Encoder<'edge>) : Encoder<MultiGraph<'edge>> =
        fun graph -> 
            Encode.object [
                "nodeCount", Encode.int (nodeCount graph)
                "edges", Graph.encode Encode.int encoder (toIntGraphSeq graph)
            ]

    let decode (decoder: Decoder<'edge>) : Decoder<MultiGraph<'edge>> =
        Decode.object <| fun get ->
            let nodeCount = get.Required.Field "nodeCount" Decode.int
            let edges = get.Required.Field "edges" (Graph.decodeArray Decode.int decoder)
            let mg = initEmpty nodeCount
            for from, edge, to' in edges do
                mg.[from, to'] <- Set.add edge mg.[from, to']
            mg

module Signature =
    let encode (encoder: Encoder<'edge>) : Encoder<Signature<'edge>> =
        fun signature -> Encode.object [
            "incoming", signature.Incoming |> Seq.map (Seq.map encoder >> Encode.seq) |> Encode.seq
            "outgoing", signature.Outgoing |> Seq.map (Seq.map encoder >> Encode.seq) |> Encode.seq
            "loops", signature.Loops |> Seq.map encoder |> Encode.seq
        ]

    let decode (decoder: Decoder<'edge>) : Decoder<Signature<'edge>> =
        Decode.object (fun get ->
            { Incoming = get.Required.Field "incoming" (Decode.immArray (Decode.set decoder))
              Outgoing = get.Required.Field "outgoing" (Decode.immArray (Decode.set decoder))
              Loops = get.Required.Field "loops" (Decode.set decoder) }
        )
