module internal TrieDict

    type Node = {
        child_nodes: Map<char, Node>
        is_word: bool 
    }

    type Trie() =
        let root : Node = { child_nodes = Map.empty; is_word = false }

        member Trie.Insert (word: string) =
            let rec insertCharacter charlist (trie_node: Node) =
                match charlist with
                | [] -> { trie_node with is_word = true} : Node // Word insertion must have ended here
                | c::cs ->
                    let node = { child_nodes = trie_node.child_nodes; is_word = false } : Node
                    let nextNode = insertCharacter cs node
                    { trie_node with child_nodes = trie_node.child_nodes |> Map.add c nextNode }
            insertCharacter (List.ofSeq word) root

        member Trie.Lookup (word: string) =
            let rec findCharacter charlist (trie_node: Node) =
                match charlist with
                | [] -> trie_node.is_word
            
            findCharacter (List.ofSeq word) root