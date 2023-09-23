namespace PatternTree

open DirectedSuMGra
open Graph
open MultiGraph
open System.Collections.Immutable

type Verdict = Positive | Negative | Neutral

type PatternTree<'pattern> =
    { Verdict: Verdict
      Pattern: 'pattern
      Children: PatternTree<'pattern> list }

module PatternTree =

    let buildQueries (tree: PatternTree<Graph<'node, 'edge>>) : PatternTree<Query<'node, 'edge>> =
        let rec buildQueries (baseQuery: Query<'node, 'edge>) i tree =
            let query = Query.extendWithGraph ($"%s{baseQuery.Id}.%d{i}") tree.Pattern baseQuery
            { Verdict = tree.Verdict
              Pattern = query
              Children = tree.Children |> List.mapi (buildQueries query) }
        let query = Query.fromGraph "root" tree.Pattern
        { Verdict = tree.Verdict
          Pattern = query
          Children = tree.Children |> List.mapi (buildQueries query) }

    let search (target: Target<'tnode, 'edge>) (tree: PatternTree<Query<'qnode, 'edge>>) : (Verdict * Query<'qnode, 'edge> * Map<int, int>) seq =
        let rec search target mapping tree =
            seq {
                for mapping in SubgraphSearch.searchExtended target tree.Pattern mapping do
                    let childResults = tree.Children |> Seq.collect (search target mapping)
                    if Seq.isEmpty childResults 
                    then yield tree.Verdict, tree.Pattern, mapping 
                    else yield! childResults
            }
        search target Map.empty tree

    let searchSimple (targetGraph: Graph<'tnode, 'edge>) (tree: PatternTree<Graph<'qnode, 'edge>>) : (Verdict * Query<'qnode, 'edge> * Map<'qnode, 'tnode>) seq =
        let target = Target.fromGraph "target" targetGraph
        search target (buildQueries tree)
        |> Seq.map (fun (verdict, query, mapping) -> 
            let mapping = 
                mapping
                |> Seq.map (fun (KeyValue (qnode, tnode)) -> query.NodeArray[qnode], target.NodeArray[tnode]) 
                |> Map.ofSeq
            verdict, query, mapping
        )
