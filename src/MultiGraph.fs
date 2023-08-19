namespace MultiGraph

open Graph
open System.Collections.Immutable

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
        Array2D.init nodeCount nodeCount (fun from to' -> Graph.edgesFromTo from to' graph)

    let toIntGraph (graph: MultiGraph<'edge>) : Graph<int, 'edge> =
        set <| seq {
            for from in 0 .. Array2D.length1 graph - 1 do
                for to' in 0 .. Array2D.length1 graph - 1 do
                    for edge in graph.[from, to'] do
                        yield from, edge, to'
        }
    
    let fromGraph (graph: Graph<'node, 'edge>) : MultiGraph<'edge> * ImmutableArray<'node> =
        let nodeArray = graph |> Graph.nodes |> ImmutableArray.ToImmutableArray
        let graph =
            Array2D.init nodeArray.Length nodeArray.Length (fun from to' -> 
                Graph.edgesFromTo nodeArray.[from] nodeArray.[to'] graph 
            )
        graph, nodeArray

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
