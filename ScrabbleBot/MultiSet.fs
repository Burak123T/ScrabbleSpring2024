// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32> 

    let empty = R Map.empty

    let isEmpty (m : MultiSet<'a>) = 
        match m with
        | R m -> Map.isEmpty m

    let size (m: MultiSet<'a>) =
        match m with
        | R m -> Map.fold (fun total key value -> total + value) 0u m

    let contains (a : 'a) (m : MultiSet<'a>) = 
        if isEmpty m then false
        else 
        match m with
        | R m -> 
            match Map.tryFind a m with
            | Some v when v > 0u -> true
            | _ -> false

    let numItems (a : 'a) (m : MultiSet<'a>) = 
        if contains a m = false then 0u
        else 
        match m with
        | R m -> 
            match Map.tryFind a m with
            | Some v -> v
            | None -> 0u

    let add (a : 'a) (n : uint32) (m : MultiSet<'a>) : MultiSet<'a> =
        let rec loop c m1 =
            if c = 0u then
                m1
            else
                match m1 with
                | R map -> loop (c - 1u) (R (Map.add a n map))

        loop n m

    let addSingle (a : 'a) (m : MultiSet<'a>) : MultiSet<'a> = add a 1u m
    
    let remove (a : 'a) (n : uint32) (m : MultiSet<'a>) : MultiSet<'a> =
        let rec loop c m1 =
            if c = 0u then
                m1
            else
                match m1 with
                | R map -> 
                    match Map.tryFind a map with
                    | Some v when v > n -> loop 0u (R (Map.add a (v - n) map))
                    | Some v -> loop (n - 1u) (R (Map.remove a map))
                    | None -> m1
        loop n m

    let removeSingle (a : 'a) (m : MultiSet<'a>) : MultiSet<'a>= remove a 1u m

    let fold (func : 'b -> 'a -> uint32 -> 'b) (start : 'b) (m : MultiSet<'a>) = 
        match m with
        | R map ->  Map.fold func start map

    let foldBack (func : 'a -> uint32 -> 'b -> 'b) (m : MultiSet<'a>) (start : 'b) = 
        match m with
        | R map -> Map.foldBack func map start

    let ofList (_ : 'a list) : MultiSet<'a> = failwith ""
    let toList (R m : MultiSet<'a>) : 'a list =
        List.collect (fun (a, n) -> List.replicate (int n) a) (Map.toList m)


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = failwith ""

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith ""
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith ""
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith ""
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = failwith ""
       
    
