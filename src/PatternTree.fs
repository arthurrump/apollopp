namespace PatternTree

open DirectedSuMGra
open Graph
open MultiGraph

type Verdict = Positive | Negative | Neutral

type PatternTree<'pattern> =
    { Verdict: Verdict
      Pattern: 'pattern
      Children: PatternTree<'pattern> list }

module PatternTree =
    open System.Collections.Immutable
    let buildMultigraphs (tree: PatternTree<Graph<'node, 'edge>>) : PatternTree<MultiGraph<'edge> * ImmutableArray<'node>> =
        let rec buildMultigraphs (baseMg, baseNodeArray) tree =
            let mg, nodeArray = MultiGraph.extendWithGraph tree.Pattern (baseMg, baseNodeArray)
            { Verdict = tree.Verdict
              Pattern = mg, nodeArray
              Children = tree.Children |> List.map (buildMultigraphs (mg, nodeArray)) }
        let mg, nodeArray = MultiGraph.fromGraph tree.Pattern
        { Verdict = tree.Verdict
          Pattern = mg, nodeArray
          Children = tree.Children |> List.map (buildMultigraphs (mg, nodeArray)) }

    let buildQueries (tree: PatternTree<MultiGraph<'edge> * ImmutableArray<'node>>) : PatternTree<Query<'edge> * ImmutableArray<'node>> =
        let rec buildQueries baseQuery tree =
            let queryGraph, nodeArray = tree.Pattern
            let query = Query.extendWithGraph queryGraph baseQuery
            { Verdict = tree.Verdict
              Pattern = query, nodeArray
              Children = tree.Children |> List.map (buildQueries query) }
        let queryGraph, nodeArray = tree.Pattern
        let query = Query.fromGraph queryGraph
        { Verdict = tree.Verdict
          Pattern = query, nodeArray
          Children = tree.Children |> List.map (buildQueries query) }

    let search (target: Target<'edge>) (tree: PatternTree<Query<'edge> * ImmutableArray<'node>>) : (Verdict * Map<int, int> * ImmutableArray<'node>) seq =
        let rec search target mapping tree =
            seq {
                let query, nodeArray = tree.Pattern
                for mapping in SubgraphSearch.searchExtended target query mapping do
                    let childResults = tree.Children |> Seq.collect (search target mapping)
                    if Seq.isEmpty childResults 
                    then yield tree.Verdict, mapping, nodeArray
                    else yield! childResults
            }
        search target Map.empty tree

    
