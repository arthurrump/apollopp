module DirectedSuMGra

open MultiGraph
open RTree
open SetTrieSetMap
open System.Collections.Immutable

module private Seq =
    open Microsoft.FSharp.Core.LanguagePrimitives
    let inline maxOrZero (s: seq<'a>) =
        if Seq.isEmpty s then GenericZero else Seq.max s
    let inline minOrZero (s: seq<'a>) =
        if Seq.isEmpty s then GenericZero else Seq.min s

module private Map =
    let inline singleton key value =
        Map.ofArray [| (key, value) |]

type SignatureIndex = RTree<int, int> 

module SignatureIndex =
    let features (signature: Signature) : int[] =
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

    let create (signatureMap: ImmutableArray<Signature>) : SignatureIndex =
        signatureMap
        |> Seq.mapi (fun i signature -> Rect.fromOriginToPoint (features signature), i)
        |> Seq.toArray
        |> RTree.createSorted 64

    let search (features: int[]) (signatureIndex: SignatureIndex) : ImmutableArray<int> =
        signatureIndex |> RTree.searchContainment (Rect.fromOriginToPoint features)

type NeighborhoodNodeIndex = 
    { IncomingIndex: SetTrieSetMap<int, int>
      OutgoingIndex: SetTrieSetMap<int, int> }
type NeighborhoodIndex = NeighborhoodNodeIndex[]

module NeighborhoodIndex =
    let create (graph: MultiGraph) : NeighborhoodIndex =
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
        |> Seq.toArray

    let search (node: int) (incomingEdges: Set<int>) (outgoingEdges: Set<int>) (neighborhoodIndex: NeighborhoodIndex) : Set<int> =
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

let private selectCandidates (signatureIndex: SignatureIndex) (queryFeatures: ImmutableArray<int[]>) (currentQueryNode: int) : ImmutableArray<int> =
    SignatureIndex.search queryFeatures.[currentQueryNode] signatureIndex

let private findJoinable (neighborhoodIndex: NeighborhoodIndex) (targetSignatureMap: ImmutableArray<Signature>) (querySignatureMap: ImmutableArray<Signature>) (orderedQuery: MultiGraph) (mapping: Map<int, int>) (currentQueryNode: int) : Set<int> =
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
        // Search the neighbourhood index from the perspective of the mapped
        // target node, so incoming edges are incoming for the mapped query node
        // and outgoing go out from the mapped query node.
        NeighborhoodIndex.search 
            mappedTargetNode 
            (orderedQuery.[currentQueryNode, mappedQueryNode]) 
            (orderedQuery.[mappedQueryNode, currentQueryNode]) 
            neighborhoodIndex
    )
    // Take the intersection of all candidate sets (for each neighbouring
    // matched query node) to find the set of all target node candidates
    // that fulfill the requirements regarding connections to the currently
    // mapped part of the graph.
    |> Set.intersectMany
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
        Set.isSuperset targetSignatureMap.[candidateTargetNode].Loops querySignatureMap.[currentQueryNode].Loops
    )

let rec private subgraphsSearch (neighborhoodIndex: NeighborhoodIndex) (targetSignatureMap: ImmutableArray<Signature>) (querySignatureMap: ImmutableArray<Signature>) (orderedQuery: MultiGraph) (mapping: Map<int, int>) (currentQueryNode: int) : Map<int, int> seq =
    let currentQueryNode = currentQueryNode + 1
    let candidateNodes = findJoinable neighborhoodIndex targetSignatureMap querySignatureMap orderedQuery mapping currentQueryNode
    seq {
        for candidateNode in candidateNodes do
            let mapping = Map.add currentQueryNode candidateNode mapping
            if currentQueryNode = MultiGraph.nodeCount orderedQuery - 1 then
                yield mapping
            else
                yield! subgraphsSearch neighborhoodIndex targetSignatureMap querySignatureMap orderedQuery mapping currentQueryNode
    } 

let query (signatureIndex: SignatureIndex) (neighborhoodIndex: NeighborhoodIndex) (targetSignatureMap: ImmutableArray<Signature>) (querySignatureMap: ImmutableArray<Signature>) (queryFeatures: ImmutableArray<int[]>) (orderedQuery: MultiGraph) =
    let initQueryNode = 0
    let initCandidateNodes = selectCandidates signatureIndex queryFeatures initQueryNode
    seq {
        for candidateNode in initCandidateNodes do
            let mapping = Map.singleton initQueryNode candidateNode
            yield! subgraphsSearch neighborhoodIndex targetSignatureMap querySignatureMap orderedQuery mapping initQueryNode
    } |> Seq.toArray

let querySimple (queryGraph: MultiGraph) (targetGraph: MultiGraph) =
    let targetSignatureMap = MultiGraph.signatureMap targetGraph
    let signatureIndex = SignatureIndex.create targetSignatureMap
    let neigborhoodIndex = NeighborhoodIndex.create targetGraph
    let orderedQuery = orderQuery queryGraph
    let querySignatureMap = MultiGraph.signatureMap orderedQuery
    let queryFeatures = 
        querySignatureMap
        |> Seq.map SignatureIndex.features 
        |> ImmutableArray.ToImmutableArray
    query signatureIndex neigborhoodIndex targetSignatureMap querySignatureMap queryFeatures orderedQuery
