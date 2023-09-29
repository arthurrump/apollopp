namespace PatternTree

open DirectedSuMGra
open Graph
open Thoth.Json.Net

type Verdict = Positive | Negative | Neutral

module Verdict =
    let encode : Encoder<Verdict> = function
        | Positive -> Encode.string "positive"
        | Negative -> Encode.string "negative"
        | Neutral -> Encode.string "neutral"

    let decode : Decoder<Verdict> =
        Decode.string
        |> Decode.andThen (function
            | "positive" -> Decode.succeed Positive
            | "negative" -> Decode.succeed Negative
            | "neutral" -> Decode.succeed Neutral
            | _ -> Decode.fail $"Invalid verdict, expected `positive`, `negative` or `neutral`"
        )

type PatternTree<'pattern> =
    { Verdict: Verdict
      Pattern: 'pattern
      Children: PatternTree<'pattern> list }

module PatternTree =

    let buildQueries (id: string) (tree: PatternTree<Graph<'node, 'edge>>) : PatternTree<Query<'node, 'edge>> =
        let rec buildQueries (baseQuery: Query<'node, 'edge>) i tree =
            let query = Query.extendWithGraph ($"%s{baseQuery.Id}.%d{i}") tree.Pattern baseQuery
            { Verdict = tree.Verdict
              Pattern = query
              Children = tree.Children |> List.mapi (buildQueries query) }
        let query = Query.fromGraph id tree.Pattern
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
        search target (buildQueries "query" tree)
        |> Seq.map (fun (verdict, query, mapping) -> 
            let mapping = 
                mapping
                |> Seq.map (fun (KeyValue (qnode, tnode)) -> query.NodeArray[qnode], target.NodeArray[tnode]) 
                |> Map.ofSeq
            verdict, query, mapping
        )

    let rec encode (encodePattern: Encoder<'pattern>) : Encoder<PatternTree<'pattern>> =
        fun tree -> Encode.object [
            "verdict", Verdict.encode tree.Verdict
            "pattern", encodePattern tree.Pattern
            if not (List.isEmpty tree.Children) then
                "children", tree.Children |> List.map (encode encodePattern) |> Encode.list
        ]

    let rec decode (decodePattern: Decoder<'pattern>) : Decoder<PatternTree<'pattern>> =
        Decode.object <| fun get ->
            { Verdict = get.Required.Field "verdict" Verdict.decode
              Pattern = get.Required.Field "pattern" decodePattern
              Children = get.Optional.Field "children" (Decode.list (decode decodePattern)) |> Option.defaultValue [] }
