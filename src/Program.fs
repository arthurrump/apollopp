module Apollopp

open RTree
open System
open System.Collections.Immutable
open System.IO
open Thoth.Json.Net

type Edge<'node, 'edge> = 'node * 'edge * 'node
type Graph<'node, 'edge when 'node : comparison and 'edge : comparison> = 
    Set<Edge<'node, 'edge>>

module Edge =
    let inline from ((from, _, _): Edge<'node, 'edge>) : 'node = from
    let inline edge ((_, edge, _): Edge<'node, 'edge>) : 'edge = edge
    let inline to' ((_, _, to'): Edge<'node, 'edge>) : 'node = to'

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

    let toStringGraph (nodeEncoder: 'node -> string) (edgeEncoder: 'edge -> string) (graph: Graph<'node, 'edge>) : Graph<string, string> =
        graph
        |> Set.map (fun (from, edge, to') -> nodeEncoder from, edgeEncoder edge, nodeEncoder to')

    let fromStringGraph (nodeDecoder: string -> 'node) (edgeDecoder: string -> 'edge) (graph: Graph<string, string>) : Graph<'node, 'edge> =
        graph
        |> Set.map (fun (from, edge, to') -> nodeDecoder from, edgeDecoder edge, nodeDecoder to')

    let encoder (nodeEncoder: Encoder<'node>) (edgeEncoder: Encoder<'edge>) : Encoder<Graph<'node, 'edge>> =
        Seq.map (Encode.tuple3 nodeEncoder edgeEncoder nodeEncoder)
        >> Encode.seq

    let decoder (nodeDecoder: Decoder<'node>) (edgeDecoder: Decoder<'edge>) : Decoder<Graph<'node, 'edge>> =
        Decode.list (Decode.tuple3 nodeDecoder edgeDecoder nodeDecoder)
        |> Decode.map (Set.ofList)

type Verdict = Positive | Negative | Neutral
type Pattern<'node, 'edge when 'node : comparison and 'edge : comparison> =
    { Verdict: Verdict
      Graph: Graph<'node, 'edge>
      Children: Pattern<'node, 'edge> list }

module Pattern =
    let extendMapping (patternExtension: Graph<'pnode, 'edge>) (mapping: Map<'pnode, 'tnode>) (target: Graph<'tnode, 'edge>) : Map<'pnode, 'tnode> array =
        patternExtension
        |> Seq.map (fun (pFrom, pEdge, pTo) ->
            match Map.tryFind pFrom mapping, Map.tryFind pTo mapping with
            // Both nodes are already mapped, if the required edge exists, the
            // mapping is valid and no extension is required
            | Some tFrom, Some tTo when Set.contains pEdge (Graph.edgesFromTo tFrom tTo target) ->
                [| Map.empty |]
            // Both nodes are already mapped, but the required edge does not
            // exist, so the mapping is not valid
            | Some _, Some _ -> 
                [||]
            | Some tFrom, None ->
                target 
                |> Seq.filter (fun (f, e, t) -> f = tFrom && e = pEdge && not (Map.containsValue t mapping))
                |> Seq.map (fun (_, _, t) -> Map.singleton pTo t)
                |> Seq.toArray
            | None, Some tTo ->
                target 
                |> Seq.filter (fun (f, e, t) -> e = pEdge && t = tTo && not (Map.containsValue f mapping))
                |> Seq.map (fun (f, _, _) -> Map.singleton pFrom f)
                |> Seq.toArray
            | None, None ->
                target 
                |> Seq.filter (fun (tFrom, tEdge, tTo) ->
                    tEdge = pEdge 
                    && not (Map.containsValue tFrom mapping) 
                    && not (Map.containsValue tTo mapping)
                )
                |> Seq.map (fun (tFrom, _, tTo) -> Map.ofArray [| (pFrom, tFrom); (pTo, tTo) |])
                |> Seq.toArray
        )
        |> Seq.fold (fun mappings mappingExtensions ->
            mappings |> Array.collect (fun mapping ->
                mappingExtensions |> Array.choose (Map.tryCombineInjective mapping)
            )
        ) ([| Map.empty |])
        |> Array.choose (Map.tryCombineInjective mapping)

    let tryFindChildMatch (pattern: Pattern<'pnode, 'edge>) (mapping: Map<'pnode, 'tnode>) (target: Graph<'tnode, 'edge>) : Option<Pattern<'pnode, 'edge> * Map<'pnode, 'tnode> array> =
        let rec helper depth pattern mapping target : Option<int * Pattern<'pnode, 'edge> * Map<'pnode, 'tnode> array> =
            let thisLevelMatches = // All children that extend the mapping with at least one valid mapping
                pattern.Children
                |> Seq.map (fun child -> child, extendMapping child.Graph mapping target)
                |> Seq.filter (fun (_, mappings) -> not (Array.isEmpty mappings))
                |> Seq.toArray

            let subMatchesByDepth =
                thisLevelMatches 
                |> Array.collect (fun (child, mappings) -> 
                    mappings |> Array.choose (fun mapping -> helper (depth + 1) child mapping target)
                )
                |> Array.groupBy (fun (depth, _, _) -> depth)

            if subMatchesByDepth |> Array.isEmpty then
                thisLevelMatches
                |> Array.tryHead
                |> Option.map (fun (child, mappings) -> depth, child, mappings)
            else
                let depth, subMatches =
                    subMatchesByDepth
                    |> Array.maxBy (fun (depth, _) -> depth)
                subMatches
                |> Seq.groupBy (fun (_, pattern, _) -> pattern)
                |> Seq.tryHead
                |> Option.map (fun (pattern, matches) -> 
                    depth, pattern, matches |> Seq.collect (fun (_, _, mappings) -> mappings) |> Seq.toArray
                )
        helper 0 pattern mapping target |> Option.map (fun (_, pattern, mappings) -> pattern, mappings)

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

type MultiGraph = Set<int>[,]
type Signature =
    { Incoming: ImmutableArray<Set<int>>
      Outgoing: ImmutableArray<Set<int>> }
    member this.Combined = this.Incoming.AddRange(this.Outgoing)

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
    
module DirectedSuMGra =
    type SignatureIndex = RTree<int, int> 
    type NeighborhoodIndex = 
        { IncomingIndex: NotImplementedException
          OutgoingIndex: NotImplementedException }

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
           signature.Combined |> Seq.map Set.count |> Seq.maxOrZero |]

    let signatureIndex (graph: MultiGraph) : SignatureIndex =
        MultiGraph.signatureMap graph
        |> Seq.mapi (fun i signature -> features signature, i)
        |> Seq.toArray
        |> RTree.create 64

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

    let selectCandidates (signatureIndex: SignatureIndex) (queryFeatures: ImmutableArray<int[]>) (queryNode: int) : ImmutableArray<int> =
        signatureIndex 
        |> RTree.search (Rect.createFromOrigin queryFeatures.[queryNode])

    let findJoinable (neighborhoodIndex: NeighborhoodIndex) (mapping: Map<int, int>) (orderedQuery: MultiGraph) (queryNode: int) : int[] =
        failwith "Not implemented"

    let rec subgraphsSearch (neighborhoodIndex: NeighborhoodIndex) (mapping: Map<int, int>) (orderedQuery: MultiGraph) (queryNode: int) (target: MultiGraph) : Map<int, int> seq =
        let queryNode = queryNode + 1
        let candidateNodes = findJoinable neighborhoodIndex mapping orderedQuery queryNode
        seq {
            for candidateNode in candidateNodes do
                let mapping = Map.add queryNode candidateNode mapping
                if queryNode = MultiGraph.nodeCount orderedQuery - 1 then
                    yield mapping
                else
                    yield! subgraphsSearch neighborhoodIndex mapping orderedQuery queryNode target
        } 

    let query (signatureIndex: SignatureIndex) (neighborhoodIndex: NeighborhoodIndex) (queryFeatures: ImmutableArray<int[]>) (orderedQuery: MultiGraph) (target: MultiGraph) =
        let initQueryNode = 0
        let initCandidateNodes = selectCandidates signatureIndex queryFeatures initQueryNode
        seq {
            for candidateNode in initCandidateNodes do
                let mapping = Map.singleton initQueryNode candidateNode
                yield! subgraphsSearch neighborhoodIndex mapping orderedQuery initQueryNode target
        } |> Seq.toArray
