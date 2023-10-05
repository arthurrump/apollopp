module Test.Json

open Expecto
open Swensen.Unquote

open System.Collections.Generic
open System.Collections.Immutable
open Thoth.Json.Net

[<Tests>]
let tests =
    testList "Json" [
        testProperty "mapArray" <| fun (dict: Map<int, int>) ->
            let result =
                Encode.dictArray Encode.int Encode.int dict 
                |> Encode.toString 0 
                |> Decode.fromString (Decode.mapArray Decode.int Decode.int) 
            test <@ result = Ok dict @>

        testProperty "immSortedDictArray" <| fun (dict: Map<int, int>) ->
            let dict = ImmutableSortedDictionary.ToImmutableSortedDictionary dict
            let result =
                Encode.dictArray Encode.int Encode.int dict 
                |> Encode.toString 0 
                |> Decode.fromString (Decode.immSortedDictArray Decode.int Decode.int) 
            test <@ 
                match result with
                | Ok result -> Seq.zip (seq result) (seq dict) |> Seq.forall (fun (a, b) -> a = b)
                | Error _ -> false 
            @>

        testProperty "array2D" <| fun (arr: int[,]) ->
            let result =
                Encode.array2D Encode.int arr
                |> Encode.toString 0
                |> Decode.fromString (Decode.array2D Decode.int)
            
            test <@ Result.isOk result @>
            match result with
            | Ok result ->
                test <@ Array2D.base1 arr = Array2D.base2 result @>
                test <@ Array2D.base2 arr = Array2D.base2 result @>
                test <@ Array2D.length1 arr = Array2D.length1 result @>
                test <@ Array2D.length2 arr = Array2D.length2 result @>
                for i = Array2D.base1 arr to Array2D.base1 arr + Array2D.length1 arr - 1 do
                    for j = Array2D.base2 arr to Array2D.base2 arr + Array2D.length2 arr - 1 do
                        test <@ arr.[i, j] = result.[i, j] @>
            | Error _ -> 
                ()

    ]
