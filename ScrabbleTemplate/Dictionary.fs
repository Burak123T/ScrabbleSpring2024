module internal Dictionary

    type Node = {
        forward_nodes: Map<char, Node>
        reverse_nodes: Map<char, Node>
        is_word: bool 
    }

    type Dict = Node

    // Root node (which is initially empty)
    let root : Node = { forward_nodes = Map.empty; reverse_nodes = Map.empty; is_word = false }

    let insert (word: string) : Dict =
        let rec insertCharacter charlist (trie_node: Node) : Dict =
            match charlist with
            | [] -> { trie_node with is_word = true} // Word insertion must have ended here
            | c::cs ->
                let next_node = Map.tryFind c trie_node.forward_nodes
                match next_node with
                | Some (n) -> 
                    let update_node = insertCharacter cs n // If node with the given character already exists
                    { trie_node with forward_nodes = trie_node.forward_nodes |> Map.add c update_node}
                | None -> 
                    let update_node = insertCharacter cs <| { is_word = false; forward_nodes = Map.empty; reverse_nodes = Map.empty }
                    { trie_node with forward_nodes = trie_node.forward_nodes |> Map.add c update_node }
        let update_root_node = insertCharacter (List.ofSeq word) root
        update_root_node

    let lookup (word: string) (d: Dict) =
        let rec findCharacter charlist (trie_node: Node) =
            match charlist with
            | [] -> trie_node.is_word
            | c::cs -> 
                let nextnode = Map.tryFind c trie_node.forward_nodes
                match nextnode with
                | Some (n) -> 
                    findCharacter cs n
                | None -> false   
        findCharacter (List.ofSeq word) root

    let step c (d: Dict) =
        match Map.tryFind c d.forward_nodes with
        | Some(_) -> Some(d.is_word, d)
        | None -> None

    let empty = { forward_nodes = Map.empty; reverse_nodes = Map.empty; is_word = false } : Dict