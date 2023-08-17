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

let private selectCandidates (signatureIndex: SignatureIndex) (queryFeatures: ImmutableArray<int[]>) (currentQueryNode: int) : ImmutableArray<int> =
    signatureIndex 
    |> RTree.search (Rect.createFromOrigin queryFeatures.[currentQueryNode])

let private findJoinable (neighborhoodIndex: NeighborhoodIndex) (mapping: Map<int, int>) (orderedQuery: MultiGraph) (currentQueryNode: int) : Set<int> =
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

let rec private subgraphsSearch (neighborhoodIndex: NeighborhoodIndex) (mapping: Map<int, int>) (orderedQuery: MultiGraph) (currentQueryNode: int) (target: MultiGraph) : Map<int, int> seq =
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

let querySimple (queryGraph: MultiGraph) (targetGraph: MultiGraph) =
    let signatureIndex = signatureIndex targetGraph
    let neigborhoodIndex = neighborhoodIndex targetGraph
    let orderedQuery = orderQuery queryGraph
    let queryFeatures = 
        MultiGraph.signatureMap orderedQuery 
        |> Seq.map features 
        |> ImmutableArray.ToImmutableArray
    query signatureIndex neigborhoodIndex queryFeatures orderedQuery targetGraph
