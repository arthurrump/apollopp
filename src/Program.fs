module Apollopp

open DirectedSuMGra
open Graph
open MultiGraph
open System.IO
open Thoth.Json.Net
open QueryBuilder

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

let targetGraph = 
    match Decode.fromString (TypeGraph.decoder Decode.string) (File.ReadAllText("D:/Arthur/Desktop/tg.json")) with
    | Ok target -> target
    | Error err -> failwith err
    
let queryGraph: TypeGraph<int> = 
    set [ (0, Annotated (InProjectDecl "java+class"), 0) 
          (1, Annotated (InProjectDecl "java+method"), 1)
          (1, Annotated (Modifier "public"), 1)
          (0, Contains, 1) ]



let targetMg, targetNodes = 
    MultiGraph.fromGraph targetGraph

let queryMg, queryNodes =
    MultiGraph.fromGraph queryGraph

let target = Target.fromGraph targetMg
let query = Query.fromGraph queryMg

let mappings = SubgraphSearch.searchSimple targetMg queryMg |> Seq.toList

for mapping in mappings do
    printfn "Mapping:"
    for key, value in mapping |> Map.toSeq |> Seq.sortBy fst do
        printfn "%2i -> %2i (%i -> %s)" key value queryNodes.[key] targetNodes.[value]

printfn "Suggested edges:"
for ext in QueryBuilder.findEdgeExtensions queryGraph (mappings |> Seq.map (fun mapping -> target, mapping)) do
    printfn "%A" ext

printfn "Suggested nodes:"
for ext in QueryBuilder.findNodeExtensions queryGraph (mappings |> Seq.map (fun mapping -> target, mapping)) do
    printfn "- %f: %A -> %A\n  Outgoing: %A\n  Incoming: %A" ext.Fraction ext.QueryNode ext.AdjacentLoops ext.Outgoing ext.Incoming
    printfn "  Occurrences:"
    for target, mapping, node in ext.Occurrences do
        printfn "  - %A: %A -> (new) %A" (target.Graph.Length) (mapping.[ext.QueryNode]) node
