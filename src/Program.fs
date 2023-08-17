module Apollopp

open Graph
open MultiGraph
open System.IO
open Thoth.Json.Net

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

let testTarget', testTargetNodes, testTargetEdges, testTargetEdgeMap = 
    MultiGraph.fromGraph testTarget

let testPattern', testPatternNodes =
    MultiGraph.fromGraphWithEdgeMap testTargetEdgeMap testPattern

let mappings = DirectedSuMGra.querySimple testPattern' testTarget'

printfn "%A" mappings

for mapping in mappings do
    printfn "Mapping:"
    for key, value in mapping |> Map.toSeq |> Seq.sortBy fst do
        printfn "%2i -> %2i (%s -> %s)" key value testPatternNodes.[key] testTargetNodes.[value]
