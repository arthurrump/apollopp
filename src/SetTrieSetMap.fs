namespace SetTrieSetMap

// Based on https://github.com/mmihaltz/pysettrie/

open System.Collections.Immutable

type SetTrieSetMap<'k, 'v when 'k : comparison and 'v : comparison> =
    { Values: Set<'v>
      Nodes: ImmutableSortedDictionary<'k, SetTrieSetMap<'k, 'v>> }

module SetTrieSetMap =
    let empty<'k, 'v when 'k : comparison and 'v : comparison> : SetTrieSetMap<'k, 'v> =
        { Values = Set.empty
          Nodes = ImmutableSortedDictionary.Empty }

    let insert (word: Set<'k>) (value: 'v) (trie: SetTrieSetMap<'k, 'v>) =
        let rec insert word value trie =
            match word with
            | [] ->
               { trie with Values = Set.add value trie.Values }
            | key::rest ->
                match trie.Nodes.TryGetValue key with
                | true, node ->
                    { trie with Nodes = trie.Nodes.SetItem(key, insert rest value node) }
                | false, _ ->
                    { trie with Nodes = trie.Nodes.Add(key, insert rest value empty) }

        insert (word |> Set.toList |> List.sort) value trie

    let create (entries: #seq<Set<'k> * 'v>) =
        entries
        |> Seq.fold (fun trie (word, value) -> insert word value trie) empty


    let search (word: Set<'k>) (trie: SetTrieSetMap<'k, 'v>) : Set<'v> =
        let rec search word trie =
            match word with
            | [] ->
                trie.Values
            | key::rest ->
                match trie.Nodes.TryGetValue key with
                | true, node -> search rest node
                | false, _ -> Set.empty

        search (word |> Set.toList |> List.sort) trie

    let rec allValues (trie: SetTrieSetMap<'k, 'v>) : Set<'v> =
        trie.Nodes |> Seq.fold (fun result (KeyValue (_, node)) -> Set.union result (allValues node)) trie.Values

    let searchSuperset (word: Set<'k>) (trie: SetTrieSetMap<'k, 'v>) : Set<'v> =
        let rec searchSuperset word trie =
            match word with
            | [] ->
                allValues trie
            | key::rest ->
                trie.Nodes 
                |> Seq.takeWhile (fun (KeyValue (k, _)) -> k <= key)
                |> Seq.map (fun (KeyValue (k, node)) -> 
                    if k = key 
                    then searchSuperset rest node
                    else searchSuperset word node
                )
                |> Set.unionMany

        searchSuperset (word |> Set.toList |> List.sort) trie
