module Dict

type Dict =
    | Leaf of bool
    | Node of bool * System.Collections.Generic.Dictionary<char, Dict>

let empty () = Leaf false

let rec insert (str: string) (dict: Dict) =
    match dict with
    | Leaf _ when str.Length = 0 -> Leaf true
    | Node(_, dict) when str.Length = 0 -> Node(true, dict)
    | Leaf b ->
        let newDict = System.Collections.Generic.Dictionary<char, Dict>()
        newDict.[str.[0]] <- insert str.[1..] (Leaf false)
        Node(b, newDict)
    | Node(b, r) ->
        let char = str.[0]

        match r.TryGetValue char with
        | (true, value) ->
            r.[char] <- insert str.[1..] value
            Node(b, r)
        | (false, _) ->
            r.[char] <- insert str.[1..] (Leaf false)
            Node(b, r)

let rec lookup (str: string) (dict: Dict) =
    //ScrabbleUtil.DebugPrint.debugPrint ("Hey I am called, bruv.")
    match dict with
    | Leaf b when str.Length = 0 -> b
    | Leaf _ -> false
    | Node(b, _) when str.Length = 0 -> b
    | Node(b, r) ->
        match r.TryGetValue str.[0] with
        | (true, next) -> lookup str.[1..] next
        | (false, _) -> false

let step (char: char) (dict: Dict) =
    //ScrabbleUtil.DebugPrint.debugPrint ("Hey I am called from ze step, bruv.")
    match dict with
    | Leaf _ -> None
    | Node(_, r) ->
        match r.TryGetValue char with
        | (true, v) ->
            match v with
            | Leaf b -> Some(b, v)
            | Node(b, _) -> Some(b, v)
        | (false, _) -> None
