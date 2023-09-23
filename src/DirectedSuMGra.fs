namespace DirectedSuMGra

// Based on SuMGra: https://doi.org/10.1007/978-3-319-44403-1_24
// Adapted to work on loopy directed graphs.

open Graph
open MultiGraph
open RTree
open SetTrieSetMap

open System.Collections.Immutable
open Thoth.Json.Net

module private Seq =
    open Microsoft.FSharp.Core.LanguagePrimitives
    let inline maxOrZero (s: seq<'a>) =
        if Seq.isEmpty s then GenericZero else Seq.max s
    let inline minOrZero (s: seq<'a>) =
        if Seq.isEmpty s then GenericZero else Seq.min s

module private Map =
    let inline singleton key value =
        Map.ofArray [| (key, value) |]

/// Nodes indexed by a set of features, used to determine the initial set of
/// candidates
type SignatureIndex = RTree<int, int> 

module SignatureIndex =
    /// Turn a node signature into a set of features to use as the key in the
    /// SignatureIndex
    let features (signature: Signature<'edge>) : int[] =
        [| // f1 Cardinality of vertex signature
           // signature.Incoming.Length + signature.Outgoing.Length
           // f1a Cardinality of incoming vertex signature
           signature.Incoming.Length
           // f1b Cardinality of outgoing vertex signature
           signature.Outgoing.Length
           // f2 The number of unique dimensions in the vertex signature
           // Seq.append signature.Incoming signature.Outgoing |> Set.unionMany |> Set.count
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

    /// Create a SignatureIndex for a graph based on its signature map
    let create (signatureMap: ImmutableArray<Signature<'edge>>) : SignatureIndex =
        signatureMap
        |> Seq.mapi (fun i signature -> Rect.fromOriginToPoint (features signature), i)
        |> Seq.toArray
        |> RTree.createSorted 64

    /// Search the SignatureIndex for nodes that contain the given features
    let search (features: int[]) (signatureIndex: SignatureIndex) : ImmutableArray<int> =
        signatureIndex |> RTree.searchContainment (Rect.fromOriginToPoint features)

    let encode : Encoder<SignatureIndex> = RTree.encode Encode.int Encode.int
    let decode : Decoder<SignatureIndex> = RTree.decode Decode.int Decode.int

/// An index of neighbouring nodes for a single node in the graph, keyed by the
/// set of multiedge labels going to or coming in from the neighbouring node
type NeighborhoodNodeIndex<'edge when 'edge : comparison> = 
    { IncomingIndex: SetTrieSetMap<'edge, int>
      OutgoingIndex: SetTrieSetMap<'edge, int> }

module NeighborhoodNodeIndex =
    let encode (encodeEdge: Encoder<'edge>) : Encoder<NeighborhoodNodeIndex<'edge>> =
        fun index -> Encode.object [
            "incoming", SetTrieSetMap.encode encodeEdge Encode.int index.IncomingIndex
            "outgoing", SetTrieSetMap.encode encodeEdge Encode.int index.OutgoingIndex
        ]

    let decode (decodeEdge: Decoder<'edge>) : Decoder<NeighborhoodNodeIndex<'edge>> =
        Decode.object (fun get ->
            { IncomingIndex = get.Required.Field "incoming" (SetTrieSetMap.decode decodeEdge Decode.int)
              OutgoingIndex = get.Required.Field "outgoing" (SetTrieSetMap.decode decodeEdge Decode.int) }
        )

/// An index of neighbouring nodes for all nodes in the graph
type NeighborhoodIndex<'edge when 'edge : comparison> = 
    ImmutableArray<NeighborhoodNodeIndex<'edge>>

module NeighborhoodIndex =
    /// Create the NeighborhoodIndex for a graph
    let create (graph: MultiGraph<'edge>) : NeighborhoodIndex<'edge> =
        let createEdgesIndex node edges =
            edges
            |> Array.mapi (fun neighbor edges -> edges, neighbor)
            // Filter out loops and nodes with no connecting edges, because they are
            // not part of the neighborhood
            |> Array.filter (fun (edges, i) -> node <> i && not (Set.isEmpty edges))
            |> SetTrieSetMap.create
        let createNodeIndex node =
            { IncomingIndex = createEdgesIndex node graph[*, node]
              OutgoingIndex = createEdgesIndex node graph[node, *] }
        [ 0 .. MultiGraph.nodeCount graph - 1 ]
        |> Seq.map createNodeIndex
        |> ImmutableArray.ToImmutableArray

    /// Search the NeighborhoodIndex for nodes that have a superset of the given
    /// incoming and outgoing edges (from the perspective of the given node)
    let search (node: int) (incomingEdges: Set<'edge>) (outgoingEdges: Set<'edge>) (neighborhoodIndex: NeighborhoodIndex<'edge>) : Set<int> =
        // We query the neighborhood index for nodes that have a *superset* of
        // the given incoming and outgoing edges from the node. If the node
        // we're looking for does not have incoming or outgoing edges from the
        // node, we search for a superset of the empty set, which should return
        // all nodes. The index only stores neighbouring nodes, however, so if
        // we ask for nodes with zero or more edges, then we just return all
        // nodes. This will only occur in the case of a disconnected query
        // graph.
        if Set.isEmpty incomingEdges && Set.isEmpty outgoingEdges then
            set [ 0 .. neighborhoodIndex.Length - 1 ]
        elif Set.isEmpty incomingEdges then
            // If the incoming edges are empty, then that would give the set of
            // all nodes. Since intersecting with the universe is rather
            // pointless, we only search the outgoing edges and just return
            // that result. 
            neighborhoodIndex.[node].OutgoingIndex |> SetTrieSetMap.searchSuperset outgoingEdges
        elif Set.isEmpty outgoingEdges then
            // And vice versa if the outgoing edges are empty.
            neighborhoodIndex.[node].IncomingIndex |> SetTrieSetMap.searchSuperset incomingEdges
        else
            // If we do have a set of edges for both, we can search both and
            // take the intersection, because we are interested in nodes that
            // include both the necessary incoming and outgoing edges.
            Set.intersect
                (neighborhoodIndex.[node].IncomingIndex |> SetTrieSetMap.searchSuperset incomingEdges)
                (neighborhoodIndex.[node].OutgoingIndex |> SetTrieSetMap.searchSuperset outgoingEdges)

    let encode (encodeEdge: Encoder<'edge>) : Encoder<NeighborhoodIndex<'edge>> =
        fun index -> index |> Seq.map (NeighborhoodNodeIndex.encode encodeEdge) |> Encode.seq

    let decode (decodeEdge: Decoder<'edge>) : Decoder<NeighborhoodIndex<'edge>> =
        Decode.immArray (NeighborhoodNodeIndex.decode decodeEdge)

/// Container for all representations of the query graph used by the subgraph
/// matching algorithm
type Query<'node, 'edge when 'edge : comparison> =
    { Id: string
      /// Signatures for each node in the query graph
      SignatureMap: ImmutableArray<Signature<'edge>>
      /// Feature vectors for each node in the query graph
      FeatureMap: ImmutableArray<int[]>
      /// Order in which the nodes of the query graph should be matched
      Order: int list
      /// The query graph itself
      Graph: MultiGraph<'edge>
      NodeArray: ImmutableArray<'node> }

module Query =
    module private Order =
        let private r1 query node = 
            let signature = MultiGraph.signature query node
            signature.Incoming.Length + signature.Outgoing.Length
        let private r2 query ordered node = 
            Set.intersect ordered (MultiGraph.adjacent node query)
            |> Set.count

        let private buildOrder (query, orderedSet, unorderedSet, startIndex) (order: int[]) =
            Seq.init (Set.count unorderedSet) (fun i -> i + startIndex)
            |> Seq.fold (fun (orderedSet, unorderedSet) i ->
                let next = 
                    unorderedSet 
                    |> Seq.groupBy (r2 query orderedSet) |> Seq.maxBy fst |> snd
                    |> Seq.maxBy (r1 query)
                order.[i] <- next
                Set.add next orderedSet, Set.remove next unorderedSet
            ) (orderedSet, unorderedSet)
            |> ignore

        /// Determine the order in which the nodes of the query graph should be
        /// matched, returning a list of node indices in that order
        let getOrder (query: MultiGraph<'edge>) =
            let nodeCount = MultiGraph.nodeCount query
            let order = Array.zeroCreate nodeCount
            order.[0] <- [ 0 .. nodeCount - 1 ] |> Seq.maxBy (r1 query)
            let orderedSet = Set.singleton order.[0]
            let unorderedSet = set [ 0 .. nodeCount - 1 ] - orderedSet
            
            buildOrder (query, orderedSet, unorderedSet, 1) order

            order |> List.ofArray

        /// Extend the given order for an extended query graph, keeping the
        /// order of the original nodes intact, such that it can be used to
        /// extend a mapping created from the base graph
        let extendOrder (baseOrder: int list) (extendedQuery: MultiGraph<'edge>) =
            let nodeCount = MultiGraph.nodeCount extendedQuery
            let baseNodeCount = List.length baseOrder
            if nodeCount < List.length baseOrder then
                invalidArg "extendedQuery" "The extended query graph must not have less nodes than the base query graph"
            elif nodeCount = List.length baseOrder then
                baseOrder
            else
                let order = Array.zeroCreate nodeCount
                baseOrder |> List.iteri (fun i node -> order.[i] <- node)
                let unorderedSet = set [ baseNodeCount .. nodeCount - 1 ]
                buildOrder (extendedQuery, set baseOrder, unorderedSet, baseNodeCount) order
                order |> List.ofArray

    /// Build all required representations from the query graph
    let fromMultiGraph (id: string) (query: MultiGraph<'edge>, nodeArray: ImmutableArray<'node>) =
        let signatureMap = MultiGraph.signatureMap query
        { Id = id
          SignatureMap = signatureMap
          FeatureMap = signatureMap |> Seq.map SignatureIndex.features |> ImmutableArray.ToImmutableArray
          Order = Order.getOrder query
          Graph = query
          NodeArray = nodeArray }

    let fromGraph (id: string) (query: Graph<'node, 'edge>) =
        fromMultiGraph id (MultiGraph.fromGraph query)

    /// Extend an existing query with an extended query graph, updating the
    /// indices, but keeping the order of the original nodes intact, such that
    /// this query can be used to extend a mapping created with the base query
    /// graph
    let extendWithMultiGraph (id: string) (extended: MultiGraph<'edge>, extendedNodeArray: ImmutableArray<'node>) (query: Query<'node, 'edge>) =
        let signatureMap = MultiGraph.signatureMap extended
        { Id = id
          SignatureMap = signatureMap
          FeatureMap = signatureMap |> Seq.map SignatureIndex.features |> ImmutableArray.ToImmutableArray
          Order = Order.extendOrder query.Order extended
          Graph = extended
          NodeArray = extendedNodeArray }
    
    let extendWithGraph (id: string) (extension: Graph<'node, 'edge>) (query: Query<'node, 'edge>) =
        extendWithMultiGraph id (MultiGraph.extendWithGraph extension (query.Graph, query.NodeArray)) query

    let encode (encodeNode: Encoder<'node>) (encodeEdge: Encoder<'edge>) : Encoder<Query<'node, 'edge>> =
        fun query -> Encode.object [
            "id", query.Id |> Encode.string
            "signatureMap", query.SignatureMap |> Seq.map (Signature.encode encodeEdge) |> Encode.seq
            "featureMap", query.FeatureMap |> Seq.map (Seq.map Encode.int >> Encode.seq) |> Encode.seq
            "order", query.Order |> List.map Encode.int |> Encode.list
            "graph", query.Graph |> MultiGraph.encode encodeEdge
            "nodeArray", query.NodeArray |> Seq.map encodeNode |> Encode.seq
        ]

    let decode (decodeNode: Decoder<'node>) (decodeEdge: Decoder<'edge>) : Decoder<Query<'node, 'edge>> =
        Decode.object (fun get ->
            { Id = get.Required.Field "id" Decode.string
              SignatureMap = get.Required.Field "signatureMap" (Decode.immArray (Signature.decode decodeEdge))
              FeatureMap = get.Required.Field "featureMap" (Decode.immArray (Decode.array Decode.int))
              Order = get.Required.Field "order" (Decode.list Decode.int)
              Graph = get.Required.Field "graph" (MultiGraph.decode decodeEdge)
              NodeArray = get.Required.Field "nodeArray" (Decode.immArray decodeNode) }
        )

/// Container for all representations of the target graph used by the subgraph
/// matching algorithm
type Target<'node, 'edge when 'edge : comparison> =
    { Id: string
      SignatureIndex: SignatureIndex
      NeighborhoodIndex: NeighborhoodIndex<'edge>
      SignatureMap: ImmutableArray<Signature<'edge>>
      Graph: MultiGraph<'edge>
      NodeArray: ImmutableArray<'node> }

module Target =
    /// Build all required representations from the target graph
    let fromMultiGraph (id: string) (target: MultiGraph<'edge>, nodeArray: ImmutableArray<'node>) =
        let signatureMap = MultiGraph.signatureMap target
        { Id = id
          SignatureIndex = SignatureIndex.create signatureMap
          NeighborhoodIndex = NeighborhoodIndex.create target
          SignatureMap = signatureMap
          Graph = target
          NodeArray = nodeArray }

    let fromGraph (id: string) (target: Graph<'node, 'edge>) =
        fromMultiGraph id (MultiGraph.fromGraph target)

    let encode (encodeNode: Encoder<'node>) (encodeEdge: Encoder<'edge>) : Encoder<Target<'node, 'edge>> =
        fun target -> Encode.object [
            "id", target.Id |> Encode.string
            "signatureIndex", target.SignatureIndex |> SignatureIndex.encode
            "neighborhoodIndex", target.NeighborhoodIndex |> NeighborhoodIndex.encode encodeEdge
            "signatureMap", target.SignatureMap |> Seq.map (Signature.encode encodeEdge) |> Encode.seq
            "graph", target.Graph |> MultiGraph.encode encodeEdge
            "nodeArray", target.NodeArray |> Seq.map encodeNode |> Encode.seq
        ]

    let decode (decodeNode: Decoder<'node>) (decodeEdge: Decoder<'edge>) : Decoder<Target<'node, 'edge>> =
        Decode.object (fun get -> 
            { Id = get.Required.Field "id" Decode.string
              SignatureIndex = get.Required.Field "signatureIndex" SignatureIndex.decode
              NeighborhoodIndex = get.Required.Field "neighborhoodIndex" (NeighborhoodIndex.decode decodeEdge)
              SignatureMap = get.Required.Field "signatureMap" (Decode.immArray (Signature.decode decodeEdge))
              Graph = get.Required.Field "graph" (MultiGraph.decode decodeEdge)
              NodeArray = get.Required.Field "nodeArray" (Decode.immArray decodeNode) }
        )

module SubgraphSearch =
    /// Select initial candidates using the SignatureIndex and verify that the
    /// loops are compatible
    let private selectInitialCandidates (target: Target<'tnode, 'edge>) (query: Query<'qnode, 'edge>) (currentQueryNode: int) =
        SignatureIndex.search query.FeatureMap.[currentQueryNode] target.SignatureIndex
        |> Seq.filter (fun candidateTargetNode ->
            Set.isSuperset target.SignatureMap.[candidateTargetNode].Loops query.SignatureMap.[currentQueryNode].Loops
        )

    /// Find all target nodes that are joinable with the already mapped query
    /// and thus are candidates to match with the current query node
    let private findJoinable (target: Target<'tnode, 'edge>) (query: Query<'qnode, 'edge>) (currentQueryNode: int) (mapping: Map<int, int>) : Set<int> =
        // Find all adjacent nodes of the current (to-be-matched) query node and
        MultiGraph.adjacent currentQueryNode query.Graph
        // filter to those that are already mapped (and include the mapped target node).
        |> Seq.choose (fun mappedQueryNode -> 
            mapping 
            |> Map.tryFind mappedQueryNode 
            |> Option.map (fun mappedTargetNode -> mappedQueryNode, mappedTargetNode)
        )
        // Select candidate target nodes based on the edges shared between
        // previously mapped nodes and the current query node
        |> Seq.map (fun (mappedQueryNode, mappedTargetNode) ->
            // Search the neighbourhood index from the perspective of the mapped
            // target node, so incoming edges are incoming for the mapped query node
            // and outgoing go out from the mapped query node.
            NeighborhoodIndex.search 
                mappedTargetNode 
                (query.Graph.[currentQueryNode, mappedQueryNode]) 
                (query.Graph.[mappedQueryNode, currentQueryNode]) 
                target.NeighborhoodIndex
        )
        // Take the intersection of all candidate sets (for each neighbouring
        // matched query node) to find the set of all target node candidates
        // that fulfill the requirements regarding connections to the currently
        // mapped part of the graph.
        // Note: Just Set.intersectMany doesn't work, because if there are no
        // adjacent nodes (can happen with a disconnected query), then this
        // fails.
        |> fun s ->
            if Seq.isEmpty s then
                // If there are no adjacent nodes, we are in basically the same
                // place as we were at the start.
                let candidates = set (selectInitialCandidates target query currentQueryNode)
                // But we do need to filter out all of the already mapped nodes,
                // to avoid mapping them twice.
                Set.difference candidates (set (Map.values mapping))
            else
                // When queries are disconnected, the given results may not be
                // valid because they can include nodes that were already
                // mapped, so filter them out
                Set.difference (Set.intersectMany s) (set (Map.values mapping))
        // TODO: We can further prune the solution space by predicting if future
        // growth of the mapping is possible, by checking if the vertex
        // signature of the current query node is contained in the signature of
        // each target node candidate. Signatures are sets of sets, and
        // containment can be modeled as a bipartite graph with nodes for each
        // set of edgeLabels in the signature and edges indicating the superset
        // relation. If there is a maximum match that matches all nodes in the
        // signature of the current query node, then we know that we can indeed
        // expand the mapping from this node to its neighbours. The maximum
        // matching can be found using Hopcroft-Karp.
        |> Set.filter (fun candidateTargetNode -> 
            // And we also need to check if the loops on the candidate node are
            // compatible with the current query node.
            Set.isSuperset target.SignatureMap.[candidateTargetNode].Loops query.SignatureMap.[currentQueryNode].Loops
        )

    /// Recursively extend the given mapping until all query nodes in the
    /// nextQueryNodes list are matched, returning the sequence of all valid
    /// mappings
    let rec private extendMapping (target: Target<'tnode, 'edge>) (query: Query<'qnode, 'edge>) (nextQueryNodes: int list) (mapping: Map<int, int>) : Map<int, int> seq =
        match nextQueryNodes with
        | currentQueryNode :: nextQueryNodes ->
            let candidateNodes = findJoinable target query currentQueryNode mapping
            seq {
                for candidateNode in candidateNodes do
                    let mapping = Map.add currentQueryNode candidateNode mapping
                    yield! extendMapping target query nextQueryNodes mapping
            }
        | [] ->
            Seq.singleton mapping

    /// Search for mappings based on target and query graph representations,
    /// returning the sequence of all valid mappings
    let search (target: Target<'tnode, 'edge>) (query: Query<'qnode, 'edge>) =
        match query.Order with
        | currentQueryNode :: nextQueryNodes ->
            let initCandidateNodes = selectInitialCandidates target query currentQueryNode
            seq {
                for candidateNode in initCandidateNodes do
                    let mapping = Map.singleton currentQueryNode candidateNode
                    yield! extendMapping target query nextQueryNodes mapping
            }
        | [] ->
            Seq.empty

    let private verifyN (count: int) (targetGraph: MultiGraph<'edge>) (queryGraph: MultiGraph<'edge>) (mapping: Map<int, int>) =
        Seq.allPairs [ 0 .. count - 1 ] [ 0 .. count - 1 ]
        |> Seq.forall (fun (qFrom, qTo) -> Set.isSubset queryGraph.[qFrom, qTo] targetGraph.[mapping.[qFrom], mapping.[qTo]])

    let verify (targetGraph: MultiGraph<'edge>) (queryGraph: MultiGraph<'edge>) (mapping: Map<int, int>) =
        verifyN (MultiGraph.nodeCount queryGraph) targetGraph queryGraph mapping

    /// Search for extended mappings based using extended query graph
    /// representations. Note: assumes that the *n* already mapped nodes are the
    /// first *n* nodes in the query node ordering. This is the case if the
    /// extended query is built using <c>Query.extendWithGraph</c>, extending
    /// the query that was used to find the mapping.
    let searchExtended (target: Target<'tnode, 'edge>) (extendedQuery: Query<'qnode, 'edge>) (mapping: Map<int, int>) =
        if verifyN (Map.count mapping) target.Graph extendedQuery.Graph mapping then
            let nextQueryNodes = extendedQuery.Order |> List.skip (Map.count mapping)
            extendMapping target extendedQuery nextQueryNodes mapping
        else
            Seq.empty

    /// Search for mappings of the query graph to the target graph based on the
    /// edgeset graph representation, returning the sequence of all valid
    /// mappings
    let searchSimpleGraph (targetGraph: Graph<'tnode, 'edge>) (queryGraph: Graph<'qnode, 'edge>) =
        let target = Target.fromGraph "target" targetGraph
        let query = Query.fromGraph "query" queryGraph
        search target query
        |> Seq.map (
            Map.toSeq
            >> Seq.map (fun (q, t) -> query.NodeArray.[q], target.NodeArray.[t]) 
            >> Map.ofSeq
        )
