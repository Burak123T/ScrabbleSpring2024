module internal Dictionary

    type Node = {
        child_nodes: Map<char, Node>
        is_word: bool 
    }

    // Root node which is initially empty
    let mutable root : Node = { child_nodes = Map.empty; is_word = false }

    let insert (word: string) =
        let rec insertCharacter charlist (trie_node: Node) =
            match charlist with
            | [] -> { trie_node with is_word = true} // Word insertion must have ended here
            | c::cs ->
                let next_node = Map.tryFind c trie_node.child_nodes
                match next_node with
                | Some (n) -> 
                    let update_node = insertCharacter cs n // If node with the given character already exists¨
                    { trie_node with child_nodes = trie_node.child_nodes |> Map.add c update_node}
                | None -> 
                    let update_node = insertCharacter cs <| { is_word = false; child_nodes = Map.empty }
                    { trie_node with child_nodes = trie_node.child_nodes |> Map.add c update_node }
        root <- insertCharacter (List.ofSeq word) root

    let lookup (word: string) =
        let rec findCharacter charlist (trie_node: Node) =
            match charlist with
            | [] -> trie_node.is_word
            | c::cs -> 
                let nextnode = Map.tryFind c trie_node.child_nodes
                match nextnode with
                | Some (n) -> 
                    findCharacter cs n
                | None -> false   
        findCharacter (List.ofSeq word) root

    let step c t (trie_node: Node)=
        match Map.tryFind c trie_node.child_nodes with
        | Some(n) -> Some(trie_node.is_word, n)
        | None -> None

    let empty =
        root <- { child_nodes = Map.empty; is_word = false }