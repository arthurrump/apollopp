module Apollopp

open Graph
open MultiGraph
open RTree
open SetTrieSetMap
open System.Collections.Immutable
open System.IO
open Thoth.Json.Net

module Seq =
    open Microsoft.FSharp.Core.LanguagePrimitives

    let inline maxOrZero (s: seq<'a>) =
        if Seq.isEmpty s then GenericZero else Seq.max s

    let inline minOrZero (s: seq<'a>) =
        if Seq.isEmpty s then GenericZero else Seq.min s

module Map =
    let singleton key value =
        Map.ofArray [| (key, value) |]

    let containsValue value map =
        Map.exists (fun _ v -> v = value) map

    let tryCombineInjective (map1: Map<'key, 'value>) (map2: Map<'key, 'value>) : Option<Map<'key, 'value>> =
        map2 |> Map.fold (fun map' key value ->
            map' |> Option.bind (fun map ->
                match Map.tryFind key map with
                // If the key is already mapped to this value, just continue
                | Some v when v = value -> 
                    Some map
                // If the key is already mapped to a different value, fail
                | Some _ -> 
                    None
                // If the value already exists in the map, fail
                | None when containsValue value map -> 
                    None
                // If the key and value are not yet in the map, add them and continue
                | None -> 
                    Some (Map.add key value map)
            )
        ) (Some map1)

type Verdict = Positive | Negative | Neutral
type Pattern<'node, 'edge when 'node : comparison and 'edge : comparison> =
    { Verdict: Verdict
      Graph: Graph<'node, 'edge>
      Children: Pattern<'node, 'edge> list }

type TypeGraphAnnotation =
    | NameClass of nameClass: string
    | Modifier of modifier: string
    | InProjectDecl of scheme: string
    | ExternalDecl of location: string

module TypeGraphAnnotation =
    let encoder : Encoder<TypeGraphAnnotation> = function
        | NameClass nameClass -> Encode.object [ "nameClass", Encode.string nameClass ]
        | Modifier modifier -> Encode.object [ "modifier", Encode.string modifier ]
        | InProjectDecl scheme -> Encode.object [ "scheme", Encode.string scheme ]
        | ExternalDecl location -> Encode.object [ "location", Encode.string location ]

    let decoder : Decoder<TypeGraphAnnotation> =
        Decode.oneOf [
            Decode.field "nameClass" Decode.string |> Decode.map NameClass
            Decode.field "modifier" Decode.string |> Decode.map Modifier
            Decode.field "scheme" Decode.string |> Decode.map InProjectDecl
            Decode.field "location" Decode.string |> Decode.map ExternalDecl
        ]

type TypeGraphEdge =
    | Extends
    | Implements
    | Invokes
    | DependsOn
    | Contains
    | Annotated of TypeGraphAnnotation

module TypeGraphEdge =
    let encoder : Encoder<TypeGraphEdge> = function
        | Extends -> Encode.string "extends"
        | Implements -> Encode.string "implements"
        | Invokes -> Encode.string "invokes"
        | DependsOn -> Encode.string "dependsOn"
        | Contains -> Encode.string "contains"
        | Annotated annotation -> Encode.object [ "annotation", TypeGraphAnnotation.encoder annotation ]

    let decoder : Decoder<TypeGraphEdge> =
        Decode.oneOf [
            Decode.string |> Decode.andThen (function
                | "extends" -> Decode.succeed Extends
                | "implements" -> Decode.succeed Implements
                | "invokes" -> Decode.succeed Invokes
                | "dependsOn" -> Decode.succeed DependsOn
                | "contains" -> Decode.succeed Contains
                | _ -> Decode.fail "Invalid edge type"
            )
            Decode.field "annotation" TypeGraphAnnotation.decoder |> Decode.map Annotated
        ]

type TypeGraph<'node when 'node : comparison> = Graph<'node, TypeGraphEdge>

module TypeGraph = 
    let encoder (nodeEncoder: Encoder<'node>) : Encoder<TypeGraph<'node>> = 
        Graph.encoder nodeEncoder TypeGraphEdge.encoder
    let decoder (nodeDecoder: Decoder<'node>) : Decoder<TypeGraph<'node>> = 
        Graph.decoder nodeDecoder TypeGraphEdge.decoder

let target = 
    match Decode.fromString (TypeGraph.decoder Decode.string) (File.ReadAllText("D:/Arthur/Desktop/tg.json")) with
    | Ok target -> target
    | Error err -> failwith err
    
let pattern: TypeGraph<int> = 
    set [ (0, Annotated (InProjectDecl "java+class"), 0) 
          (1, Annotated (InProjectDecl "java+method"), 1)
          (1, Annotated (Modifier "public"), 1)
          (0, Contains, 1) ]

let testTarget = set [
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

let testPattern = set [
    ("p0", "class", "p0")
    ("p1", "method", "p1")
    ("p1", "public", "p1")
    ("p0", "contains", "p1")
]

module DirectedSuMGra =
    type SignatureIndex = RTree<int, int> 

    type NeighborhoodNodeIndex = 
        { IncomingIndex: SetTrieSetMap<int, int>
          OutgoingIndex: SetTrieSetMap<int, int> }
    type NeighborhoodIndex = NeighborhoodNodeIndex[]

    let features (signature: Signature) : int[] =
        [| // f1 Cardinality of vertex signature
           // signature.Incoming.Count + signature.Outgoing.Count
           // f1a Cardinality of incoming vertex signature
           signature.Incoming.Length
           // f1b Cardinality of outgoing vertex signature
           signature.Outgoing.Length
           // f2 The number of unique dimensions in the vertex signature
           // Set.union signature.Incoming signature.Outgoing |> Set.unionMany |> Set.count
           // f2a The number of unique dimensions in the incoming vertex signature
           signature.Incoming |> Set.unionMany |> Set.count
           // f2b The number of unique dimensions in the outgoing vertex signature
           signature.Outgoing |> Set.unionMany |> Set.count
           // f3 The number of all occurrences of the dimensions (repetition allowed)
           (Seq.sumBy Set.count signature.Incoming) + (Seq.sumBy Set.count signature.Outgoing)
           // f4 Minimum index of the lexicographically ordered edge dimensions
           // "the field f4 contains the minimum value of the index, and hence we
           // negate f4 so that the rectangular containment problem still holds
           // good"
           // -1 * (signature |> Seq.map Set.minElement |> Seq.minOrZero)
           // f5 Maximum index of the lexicographically ordered edge dimensions
           // signature |> Seq.map Set.maxElement |> Seq.maxOrZero
           // f6 Maximum cardinality of the vertex sub-signature
           Seq.append signature.Incoming signature.Outgoing |> Seq.map Set.count |> Seq.maxOrZero |]

    let signatureIndex (graph: MultiGraph) : SignatureIndex =
        MultiGraph.signatureMap graph
        |> Seq.mapi (fun i signature -> features signature, i)
        |> Seq.toArray
        |> RTree.create 64

    let neighborhoodIndex (graph: MultiGraph) : NeighborhoodIndex =
        let createEdgesIndex edges =
            edges
            |> Array.mapi (fun neighbor edges -> edges, neighbor)
            |> Array.filter (fun (edges, _) -> Set.isEmpty edges)
            |> SetTrieSetMap.create
        let createNodeIndex node =
            { IncomingIndex = createEdgesIndex graph[*, node] 
              OutgoingIndex = createEdgesIndex graph[node, *] }
        [ 0 .. MultiGraph.nodeCount graph - 1 ]
        |> Seq.map createNodeIndex
        |> Seq.toArray

    let orderQuery (query: MultiGraph) : MultiGraph =
        let r1 node = 
            let signature = MultiGraph.signature query node
            signature.Incoming.Length + signature.Outgoing.Length
        let r2 ordered node = 
            Set.intersect ordered (MultiGraph.adjacent node query)
            |> Set.count
        
        let nodeCount = MultiGraph.nodeCount query
        let order = Array.zeroCreate nodeCount
        order.[0] <- [ 0 .. nodeCount - 1 ] |> Seq.maxBy r1
        let orderedSet = Set.singleton order.[0]
        let unorderedSet = set [ 0 .. nodeCount - 1 ] - orderedSet
        
        Seq.init (nodeCount - 1) (fun i -> i + 1)
        |> Seq.fold (fun (orderedSet, unorderedSet) i ->
            let next = 
                unorderedSet 
                |> Seq.groupBy (r2 orderedSet) |> Seq.maxBy fst |> snd
                |> Seq.maxBy r1
            order.[i] <- next
            Set.add next orderedSet, Set.remove next unorderedSet
        ) (orderedSet, unorderedSet)
        |> ignore

        let orderedQuery = MultiGraph.initEmpty nodeCount
        for from in [ 0 .. nodeCount - 1 ] do
            for to' in [ 0 .. nodeCount - 1 ] do
                orderedQuery.[from, to'] <- query.[order.[from], order.[to']]
        orderedQuery

    let selectCandidates (signatureIndex: SignatureIndex) (queryFeatures: ImmutableArray<int[]>) (currentQueryNode: int) : ImmutableArray<int> =
        signatureIndex 
        |> RTree.search (Rect.createFromOrigin queryFeatures.[currentQueryNode])

    let findJoinable (neighborhoodIndex: NeighborhoodIndex) (mapping: Map<int, int>) (orderedQuery: MultiGraph) (currentQueryNode: int) : Set<int> =
        // Find all adjacent nodes of the current (to-be-matched) query node and
        MultiGraph.adjacent currentQueryNode orderedQuery
        // filter to those that are already mapped (and include the mapped target node).
        |> Seq.choose (fun mappedQueryNode -> 
            mapping 
            |> Map.tryFind mappedQueryNode 
            |> Option.map (fun mappedTargetNode -> mappedQueryNode, mappedTargetNode)
        )
        // Select candidate target nodes based on the edges shared between
        // previously mapped nodes and the current query node
        |> Seq.map (fun (mappedQueryNode, mappedTargetNode) ->
            // Only nodes that have both the required incoming and outgoing
            // edges are valid candidates, so we take the intersection of the
            // two candidate sets based on the neigborhood index.
            Set.intersect
                // Use the neighborhood index of a mapped target node to find
                // all target nodes with outgoing edges that are a superset of
                // the edges that come into that mapped target node. All target
                // nodes that have these edges are candidates to map to the
                // current query node.
                (neighborhoodIndex.[mappedTargetNode].IncomingIndex |> SetTrieSetMap.searchSuperset orderedQuery.[currentQueryNode, mappedQueryNode])
                // And vice versa for incoming edges that go out from the mapped
                // target node.
                (neighborhoodIndex.[mappedTargetNode].OutgoingIndex |> SetTrieSetMap.searchSuperset orderedQuery.[mappedQueryNode, currentQueryNode])
        )
        // Take the intersection of all candidate sets (for each neighbouring
        // matched query node) to find the set of all target node candidates
        // that fulfill the requirements regarding connections to the currently
        // mapped part of the graph.
        |> Set.intersectMany
        // TODO: We can further prune the solution space by predicting if future
        // growth of the mapping is possible, by checking if the vertex
        // signature of the current query node is contained in the signatuer of
        // each target node candidate. Signatures are sets of sets, and
        // containment can be modeled as a bipartite graph with nodes for each
        // set of edgeLabels in the signature and edges indicating the superset
        // relation. If there is a maximum match that matches all nodes in the
        // signature of the current query node, then we know that we can indeed
        // expand the mapping from this node to its neighbours. The maximum
        // matching can be found using Hopcroft-Karp.
        // |> Set.filter (fun candidateTargetNode -> ...)

    let rec subgraphsSearch (neighborhoodIndex: NeighborhoodIndex) (mapping: Map<int, int>) (orderedQuery: MultiGraph) (currentQueryNode: int) (target: MultiGraph) : Map<int, int> seq =
        let currentQueryNode = currentQueryNode + 1
        let candidateNodes = findJoinable neighborhoodIndex mapping orderedQuery currentQueryNode
        seq {
            for candidateNode in candidateNodes do
                let mapping = Map.add currentQueryNode candidateNode mapping
                if currentQueryNode = MultiGraph.nodeCount orderedQuery - 1 then
                    yield mapping
                else
                    yield! subgraphsSearch neighborhoodIndex mapping orderedQuery currentQueryNode target
        } 

    let query (signatureIndex: SignatureIndex) (neighborhoodIndex: NeighborhoodIndex) (queryFeatures: ImmutableArray<int[]>) (orderedQuery: MultiGraph) (target: MultiGraph) =
        let initQueryNode = 0
        let initCandidateNodes = selectCandidates signatureIndex queryFeatures initQueryNode
        seq {
            for candidateNode in initCandidateNodes do
                let mapping = Map.singleton initQueryNode candidateNode
                yield! subgraphsSearch neighborhoodIndex mapping orderedQuery initQueryNode target
        } |> Seq.toArray
