module internal TrieDict

    type Node = {
        child_nodes: Map<char, Node>
        is_word: bool 
    }

    type Trie() =

        // Root node which is initially empty
        let mutable root : Node = { child_nodes = Map.empty; is_word = false }

        member Trie.Insert (word: string) =
            let rec insertCharacter charlist (trie_node: Node) =
                match charlist with
                | [] -> { trie_node with is_word = true} : Node // Word insertion must have ended here
                | c::cs ->
                    let nextNode = insertCharacter cs trie_node
                    { trie_node with child_nodes = trie_node.child_nodes |> Map.add c nextNode }
            root <- insertCharacter (List.ofSeq word) root

        member Trie.Lookup (word: string) =
            let rec findCharacter charlist (trie_node: Node) =
                match charlist with
                | [] -> trie_node.is_word
                | c::cs -> 
                    let nextnode = Map.tryFind c trie_node.child_nodes
                    match nextnode with
                    | Some (n) -> 
                        printfn("Found Some n")
                        findCharacter cs n
                    | None -> findCharacter cs trie_node    
            findCharacter (List.ofSeq word) root