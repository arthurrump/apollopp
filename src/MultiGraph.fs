namespace MultiGraph

open Graph
open System.Collections.Immutable

type MultiGraph = Set<int>[,]
type Signature =
    { Incoming: ImmutableArray<Set<int>>
      Outgoing: ImmutableArray<Set<int>> }

module MultiGraph =
    let initEmpty (nodeCount: int) : MultiGraph = 
        Array2D.create nodeCount nodeCount Set.empty

    let fromIntGraph (graph: Graph<int, int>) : MultiGraph =
        let nodeCount = graph |> Graph.nodes |> Set.count
        Array2D.init nodeCount nodeCount (fun from to' -> Graph.edgesFromTo from to' graph)
    
    let fromGraph (graph: Graph<'node, 'edge>) : MultiGraph * ImmutableArray<'node> * ImmutableArray<'edge> =
        let edgeArray = graph |> Graph.edges |> ImmutableArray.ToImmutableArray
        let nodeArray = graph |> Graph.nodes |> ImmutableArray.ToImmutableArray
        let edgeMap = edgeArray |> Seq.mapi (fun index edge -> edge, index) |> Map.ofSeq
        let graph =
            Array2D.init nodeArray.Length nodeArray.Length (fun from to' -> 
                Graph.edgesFromTo nodeArray.[from] nodeArray.[to'] graph 
                |> Set.map (fun edge -> edgeMap.[edge]) 
            )
        graph, nodeArray, edgeArray

    let nodeCount : MultiGraph -> int = Array2D.length1

    let getMultiEdge (from: int) (to': int) (graph: MultiGraph) : Set<int> =
        graph.[from, to']

    let adjacent (node: int) (graph: MultiGraph) =
        let filterConnections =
            Array.mapi (fun to' edges -> to', edges) 
            >> Array.choose (fun (to', edges) -> if Set.isEmpty edges then None else Some to')
        Set.union 
            (set (filterConnections graph.[node, *]))
            (set (filterConnections graph.[*, node]))

    let signature (graph: MultiGraph) (node: int) : Signature =
        { Incoming = 
            graph.[*, node] |> Seq.filter (not << Set.isEmpty) |> ImmutableArray.ToImmutableArray
          Outgoing = 
            graph.[node, *] |> Seq.filter (not << Set.isEmpty) |> ImmutableArray.ToImmutableArray }

    let signatureMap (graph: MultiGraph) : ImmutableArray<Signature> =
        Seq.init (nodeCount graph) (signature graph)
        |> ImmutableArray.ToImmutableArray