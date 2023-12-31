module Test.SetTrieSetMap

open Expecto
open FsCheck
open Swensen.Unquote

open SetTrieSetMap

[<Tests>]
let tests =
    testList "SetTrieSetMap" [
        testProperty "Search finds all entries" <| fun (entries: (Set<int> * int) list) ->
            let trie = SetTrieSetMap.create entries
            for keys, value in entries do
                let result = SetTrieSetMap.search keys trie
                test <@ Set.contains value result @>

        testProperty "Search finds all uniquely keyed entries exactly" <| fun (entries: Map<Set<int>, int>) ->
            let entries = entries |> Map.toSeq
            let trie = SetTrieSetMap.create entries
            for keys, value in entries do
                let result = SetTrieSetMap.search keys trie
                test <@ result = Set.singleton value @>
        
        testProperty "Superset search finds all entries with superset keys when searched with one or two keys" <|
            fun (entries: (Set<int> * int) list) ->
                let trie = SetTrieSetMap.create entries
                let allKeys = entries |> List.map fst |> Set.unionMany
                let keySets = 
                    Seq.allPairs allKeys allKeys 
                    |> Seq.map (fun (key1, key2) -> set [ key1; key2 ])
                for keys in keySets do
                        test <@
                            SetTrieSetMap.searchSuperset keys trie
                                = (entries |> List.filter (fun (k, _) -> Set.isSuperset k keys) |> List.map snd |> Set.ofList)
                        @>

        testProperty "Subset search finds all entries with union of all keys" <| fun (entries: (Set<int> * int) list) ->
            let trie = SetTrieSetMap.create entries
            let allKeys = entries |> List.map fst |> Set.unionMany
            test <@
                SetTrieSetMap.searchSubset allKeys trie
                    = (entries |> List.map snd |> Set.ofList)
            @>

        testProperty "Subset search finds all entries with subset keys" <| fun (entries: (Set<int> * int) list) ->
            let trie = SetTrieSetMap.create entries
            let allKeys = entries |> List.map fst |> Set.unionMany
            Prop.forAll (Gen.subListOf allKeys |> Gen.map set |> Arb.fromGen) <| fun (keys: Set<int>) ->
                test <@
                    SetTrieSetMap.searchSubset keys trie
                        = (entries |> List.filter (fun (k, _) -> Set.isSubset k keys) |> List.map snd |> Set.ofList)
                @>
    ]
