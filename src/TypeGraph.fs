namespace TypeGraph

open Graph
open Thoth.Json.Net

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
    | Overrides
    | AccessesField
    | DependsOn
    | Contains
    | Annotated of TypeGraphAnnotation

module TypeGraphEdge =
    let encoder : Encoder<TypeGraphEdge> = function
        | Extends -> Encode.string "extends"
        | Implements -> Encode.string "implements"
        | Invokes -> Encode.string "invokes"
        | Overrides -> Encode.string "overrides"
        | AccessesField -> Encode.string "accessesField"
        | DependsOn -> Encode.string "dependsOn"
        | Contains -> Encode.string "contains"
        | Annotated annotation -> Encode.object [ "annotation", TypeGraphAnnotation.encoder annotation ]

    let decoder : Decoder<TypeGraphEdge> =
        Decode.oneOf [
            Decode.string |> Decode.andThen (function
                | "extends" -> Decode.succeed Extends
                | "implements" -> Decode.succeed Implements
                | "invokes" -> Decode.succeed Invokes
                | "overrides" -> Decode.succeed Overrides
                | "accessesField" -> Decode.succeed AccessesField
                | "dependsOn" -> Decode.succeed DependsOn
                | "contains" -> Decode.succeed Contains
                | _ -> Decode.fail "Invalid edge type"
            )
            Decode.field "annotation" TypeGraphAnnotation.decoder |> Decode.map Annotated
        ]

type TypeGraph<'node when 'node : comparison> = Graph<'node, TypeGraphEdge>

module TypeGraph = 
    let encoder (nodeEncoder: Encoder<'node>) : Encoder<TypeGraph<'node>> = 
        Graph.encode nodeEncoder TypeGraphEdge.encoder
    let decoder (nodeDecoder: Decoder<'node>) : Decoder<TypeGraph<'node>> = 
        Graph.decode nodeDecoder TypeGraphEdge.decoder
