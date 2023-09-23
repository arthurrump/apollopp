namespace Thoth.Json.Net

open System.Collections.Generic
open System.Collections.Immutable

module Encode =
    let dictArray (encodeKey: Encoder<'k>) (encodeValue: Encoder<'v>) : Encoder<IReadOnlyDictionary<'k, 'v>> =
        Seq.map (fun (KeyValue (key, value)) -> Encode.tuple2 encodeKey encodeValue (key, value))
        >> Encode.seq

    let array2D (encoder: Encoder<'t>) : Encoder<'t[,]> =
        fun arr -> Encode.object [
            "base1", Encode.int (Array2D.base1 arr)
            "base2", Encode.int (Array2D.base2 arr)
            "length1", Encode.int (Array2D.length1 arr)
            "length2", Encode.int (Array2D.length2 arr)
            "values", Encode.seq <| seq {
                for i = Array2D.base1 arr to Array2D.base1 arr + Array2D.length1 arr - 1 do
                    for j = Array2D.base2 arr to Array2D.base2 arr + Array2D.length2 arr - 1 do
                        yield encoder arr.[i, j]
            }
        ]

module Decode =
    let immArray (decoder: Decoder<'t>) : Decoder<ImmutableArray<'t>> = 
        Decode.array decoder 
        |> Decode.map ImmutableArray.ToImmutableArray

    let set (decoder: Decoder<'t>) : Decoder<Set<'t>> =
        Decode.array decoder
        |> Decode.map Set.ofArray

    let mapArray (decodeKey: Decoder<'k>) (decodeValue: Decoder<'v>) : Decoder<Map<'k, 'v>> =
        Decode.array (Decode.tuple2 decodeKey decodeValue)
        |> Decode.map Map.ofSeq

    let immSortedDictArray (decodeKey: Decoder<'k>) (decodeValue: Decoder<'v>) : Decoder<ImmutableSortedDictionary<'k, 'v>> =
        Decode.array (Decode.tuple2 decodeKey decodeValue |> Decode.map KeyValuePair)
        |> Decode.map ImmutableSortedDictionary.CreateRange

    let array2D (decoder: Decoder<'t>) : Decoder<'t[,]> = 
        Decode.object (fun get ->
            let base1 = get.Required.Field "base1" Decode.int
            let base2 = get.Required.Field "base2" Decode.int
            let length1 = get.Required.Field "length1" Decode.int
            let length2 = get.Required.Field "length2" Decode.int
            get.Required.Raw (Decode.field "values" (
                Decode.array decoder 
                |> Decode.map (fun arr ->
                    Array2D.initBased base1 base2 length1 length2 (fun i j ->
                        arr.[i * length2 + j]
                    )
                )
            ))
        )
