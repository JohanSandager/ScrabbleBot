module internal MultiSet

    type MultiSet<'a when 'a : comparison> = R of Map<'a, uint32> // replace with your type

    let empty = R (Map.empty)

    let isEmpty (s : MultiSet<'a>) = 
        match s with 
        | R s -> s.IsEmpty 

    let size (R (s)) = Map.fold (fun acc key value-> acc + value) 0u s
    
    let contains (a : 'a) (R (s)) = s.ContainsKey a

    let numItems (a : 'a) (R (s)) = s.TryFind a |> Option.defaultValue 0u

    let add (a : 'a) (n : uint32) (R (s)) : MultiSet<'a> = R(s.Add(a, ((numItems a (R(s))) + n)))

    let addSingle (a : 'a) (s : MultiSet<'a>) : MultiSet<'a> = add a 1u s
    
    let remove (a : 'a) (n : uint32) (R(s)) : MultiSet<'a> = 
        let occurences = numItems a (R(s))
        match occurences with
        | x when x > n -> R(s.Add(a, (occurences - n)))
        | _ -> R(s.Remove a)

    let removeSingle (a : 'a) (s : MultiSet<'a>) : MultiSet<'a> = remove a 1u s

    let fold (f : 'b -> 'a -> uint32 -> 'b) (acc : 'b) (R(s)) = Map.fold f acc s
    let foldBack (f : 'a -> uint32 -> 'b -> 'b) (R(s)) (acc: 'b) = Map.foldBack f s acc
    
    let ofList (_ : 'a list) : MultiSet<'a> = empty
    let toList (_ : MultiSet<'a>) : 'a list = []


    let map (_ : 'a -> 'b) (_ : MultiSet<'a>) : MultiSet<'b> = empty

    let union (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    let sum (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let subtract (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
    
    let intersection (_ : MultiSet<'a>) (_ : MultiSet<'a>) : MultiSet<'a> = empty
       
    