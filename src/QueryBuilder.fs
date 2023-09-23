namespace QueryBuilder

open DirectedSuMGra
open Graph
open MultiGraph

module Map =
    let reverseUnique (map: Map<'key, 'value>) =
        map |> Seq.map (fun (KeyValue (key, value)) -> value, key) |> Map.ofSeq

type EdgeExtension<'edge> =
    { Edge: Edge<int, 'edge>
      /// The fraction of mapped targets that contain this edge
      Fraction: float }

type NodeExtension<'node, 'edge when 'edge : comparison> =
    { QueryNode: int
      AdjacentLoops: Set<'edge>
      Outgoing: Set<'edge>
      Incoming: Set<'edge>
      Fraction: float
      Occurrences: (Target<'node, 'edge> * Map<int, int> * int) list }

module QueryBuilder =
    let findEdgeExtensions (query: Graph<int, 'edge>) (targetMappings: (Target<'tnode, 'edge> * Map<int, int>) seq) =
        let targetCount = targetMappings |> Seq.length
        let extensions = seq {
            for target, mapping in targetMappings do
                let reverseMapping = Map.reverseUnique mapping
                let inducedQuery = 
                    target.Graph 
                    |> MultiGraph.inducedGraph (Map.values mapping)
                    |> Graph.mapNode (fun node -> reverseMapping.[node])
                yield! Set.difference inducedQuery query
        }
        extensions
        |> Seq.countBy id
        |> Seq.map (fun (extension, count) -> { Edge = extension; Fraction = float count / float targetCount })
        |> Seq.sortByDescending (fun e -> e.Fraction)

    let findNodeExtensions (query: Graph<int, 'edge>) (targetMappings: (Target<'tnode, 'edge> * Map<int, int>) seq) =
        let targetCount = targetMappings |> Seq.length
        seq {
            for queryNode in query |> Graph.nodes do
                for target, mapping in targetMappings do
                    let mappedTargetNode = mapping.[queryNode]
                    let adjacent = target.Graph |> MultiGraph.adjacent mappedTargetNode
                    for extension in Set.difference adjacent (set (Map.values mapping)) do
                        yield queryNode, target.Graph[extension, extension], target.Graph.[mappedTargetNode, extension], target.Graph[extension, mappedTargetNode], target, mapping, extension
        }
        |> Seq.groupBy (fun (node, adjacentLoops, outgoing, incoming, _, _, _) -> node, adjacentLoops, outgoing, incoming)
        |> Seq.map (fun ((node, adjacentLoops, outgoing, incoming), extensions) ->
            let occurences =
                extensions 
                |> Seq.map (fun (_, _, _, _, target, mapping, targetNode) -> target, mapping, targetNode) 
                |> Seq.distinctBy (fun (target, mapping, targetNode) -> target.Graph, mapping, targetNode)
                |> Seq.toList
            { QueryNode = node
              AdjacentLoops = adjacentLoops
              Outgoing = outgoing
              Incoming = incoming
              Fraction = float (Seq.length <| Seq.distinctBy (fun (target, mapping, _) -> target.Graph, mapping) occurences) / float targetCount
              Occurrences = occurences }
        )
        |> Seq.sortByDescending (fun n -> n.Fraction)
