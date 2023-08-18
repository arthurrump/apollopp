namespace MultiGraph

open Graph
open System.Collections.Immutable

type MultiGraph = Set<int>[,]
type Signature =
    { Incoming: ImmutableArray<Set<int>>
      Outgoing: ImmutableArray<Set<int>>
      Loops: Set<int> }

module MultiGraph =
    let initEmpty (nodeCount: int) : MultiGraph = 
        Array2D.create nodeCount nodeCount Set.empty

    let fromIntGraph (graph: Graph<int, int>) : MultiGraph =
        let nodeCount = graph |> Graph.nodes |> Set.count
        Array2D.init nodeCount nodeCount (fun from to' -> Graph.edgesFromTo from to' graph)

    let toIntGraph (graph: MultiGraph) : Graph<int, int> =
        set <| seq {
            for from in 0 .. Array2D.length1 graph - 1 do
                for to' in 0 .. Array2D.length1 graph - 1 do
                    for edge in graph.[from, to'] do
                        yield from, edge, to'
        }
    
    let fromGraph (graph: Graph<'node, 'edge>) : MultiGraph * ImmutableArray<'node> * ImmutableArray<'edge> * Map<'edge, int> =
        let edgeArray = graph |> Graph.edges |> ImmutableArray.ToImmutableArray
        let nodeArray = graph |> Graph.nodes |> ImmutableArray.ToImmutableArray
        let edgeMap = edgeArray |> Seq.mapi (fun index edge -> edge, index) |> Map.ofSeq
        let graph =
            Array2D.init nodeArray.Length nodeArray.Length (fun from to' -> 
                Graph.edgesFromTo nodeArray.[from] nodeArray.[to'] graph 
                |> Set.map (fun edge -> edgeMap.[edge]) 
            )
        graph, nodeArray, edgeArray, edgeMap

    let toGraph (nodeArray: ImmutableArray<'node>) (edgeArray: ImmutableArray<'edge>) (graph: MultiGraph) : Graph<'node, 'edge> =
        set <| seq {
            for from in 0 .. Array2D.length1 graph - 1 do
                for to' in 0 .. Array2D.length1 graph - 1 do
                    for edge in graph.[from, to'] do
                        yield nodeArray.[from], edgeArray.[edge], nodeArray.[to']
        }

    let fromGraphWithEdgeMap (edgeMap: Map<'edge, int>) (graph: Graph<'node, 'edge>) : MultiGraph * ImmutableArray<'node> =
        let nodeArray = graph |> Graph.nodes |> ImmutableArray.ToImmutableArray
        let graph =
            Array2D.init nodeArray.Length nodeArray.Length (fun from to' ->
                Graph.edgesFromTo nodeArray.[from] nodeArray.[to'] graph
                |> Set.map (fun edge -> edgeMap.[edge])
            )
        graph, nodeArray

    let nodeCount : MultiGraph -> int = Array2D.length1

    let getMultiEdge (from: int) (to': int) (graph: MultiGraph) : Set<int> =
        graph.[from, to']

    let adjacent (node: int) (graph: MultiGraph) =
        let filterConnections =
            Array.mapi (fun to' edges -> to', edges) 
            >> Array.choose (fun (to', edges) -> if Set.isEmpty edges || to' = node then None else Some to')
        Set.union 
            (set (filterConnections graph.[node, *]))
            (set (filterConnections graph.[*, node]))

    let signature (graph: MultiGraph) (node: int) : Signature =
        let filterEdges edges =
            edges
            |> Seq.indexed
            |> Seq.filter (fun (i, edges) -> not (Set.isEmpty edges || i = node))
            |> Seq.map snd
            |> ImmutableArray.ToImmutableArray
        { Incoming = filterEdges graph.[*, node]
          Outgoing = filterEdges graph.[node, *]
          Loops = graph.[node, node] }

    let signatureMap (graph: MultiGraph) : ImmutableArray<Signature> =
        Seq.init (nodeCount graph) (signature graph)
        |> ImmutableArray.ToImmutableArray
