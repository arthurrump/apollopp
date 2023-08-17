namespace Graph

open Thoth.Json.Net

type Edge<'node, 'edge> = 'node * 'edge * 'node

module Edge =
    let inline from ((from, _, _): Edge<'node, 'edge>) : 'node = from
    let inline edge ((_, edge, _): Edge<'node, 'edge>) : 'edge = edge
    let inline to' ((_, _, to'): Edge<'node, 'edge>) : 'node = to'

type Graph<'node, 'edge when 'node : comparison and 'edge : comparison> = 
    Set<Edge<'node, 'edge>>

module Graph =
    // O(N*log(N))
    let inline from (graph: Graph<'node, 'edge>) : Set<'node> =
        Set.map Edge.from graph

    // O(N*log(N))
    let inline to' (graph: Graph<'node, 'edge>) : Set<'node> =
        Set.map Edge.to' graph

    // O(N*log(N))
    let inline nodes (graph: Graph<'node, 'edge>) : Set<'node> =
        Set.union (from graph) (to' graph)

    // O(N*log(N))
    let inline edges (graph: Graph<'node, 'edge>) : Set<'edge> =
        Set.map Edge.edge graph

    // O(N)
    let inline edgesFrom (node: 'node) (graph: Graph<'node, 'edge>) : Graph<'node, 'edge> =
        Set.filter (fun (from, _, _) -> from = node) graph

    // O(N)
    let inline edgesTo (node: 'node) (graph: Graph<'node, 'edge>) : Graph<'node, 'edge> =
        Set.filter (fun (_, _, to') -> to' = node) graph

    // O(N*log(N))
    let inline edgesFromTo (from: 'node) (to': 'node) (graph: Graph<'node, 'edge>) : Set<'edge> =
        Set.filter (fun (f, _, t) -> f = from && t = to') graph
        |> Set.map Edge.edge

    // O(N)
    let inline connectedEdges (node: 'node) (graph: Graph<'node, 'edge>) : Graph<'node, 'edge> =
        Set.filter (fun (from, _, to') -> node = from || node = to') graph

    // O(N*log(N))
    let inline allConnectedEdges (nodes: Set<'node>) (graph: Graph<'node, 'edge>) : Graph<'node, 'edge> =
        Set.filter (fun (from, _, to') -> Set.contains from nodes || Set.contains to' nodes) graph

    let inline extendedSubgraph (subgraph: Graph<'node, 'edge>) (graph: Graph<'node, 'edge>) : Graph<'node, 'edge> =
        allConnectedEdges (nodes subgraph) graph

    // O(N*log(N))
    let inline subgraphExtension (subgraph: Graph<'node, 'edge>) (graph: Graph<'node, 'edge>) : Graph<'node, 'edge> =
        Set.difference (extendedSubgraph subgraph graph) subgraph

    // O(N*log(N))
    let inline combine (graph1: Graph<'node, 'edge>) (graph2: Graph<'node, 'edge>) : Graph<'node, 'edge> =
        Set.union graph1 graph2

    let encoder (nodeEncoder: Encoder<'node>) (edgeEncoder: Encoder<'edge>) : Encoder<Graph<'node, 'edge>> =
        Seq.map (Encode.tuple3 nodeEncoder edgeEncoder nodeEncoder)
        >> Encode.seq

    let decoder (nodeDecoder: Decoder<'node>) (edgeDecoder: Decoder<'edge>) : Decoder<Graph<'node, 'edge>> =
        Decode.list (Decode.tuple3 nodeDecoder edgeDecoder nodeDecoder)
        |> Decode.map (Set.ofList)