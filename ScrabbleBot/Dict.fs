module Dict

type Dict =
    | Leaf of bool //(* empty csDict)
    | Node of bool * System.Collections.Generic.Dictionary<char, Dict>

type csDict = System.Collections.Generic.Dictionary<char, Dict>

let empty () = Leaf false


(* let rec insert (str: string) (dict: Dict) =
    match dict with
    | Leaf _ when str.Length = 0 -> Leaf true
    | Node(_, r) when str.Length = 0 -> Node(true, r)
    | Leaf b -> Node(b, Map.add str.[0] (insert str.[1..] (Leaf false)) Map.empty)
    | Node(b, r) ->
        let nextDict =
            match r.TryGetValue str.[0] with
            | (true, next) -> next
            | (false, _) -> Leaf false

        Node(b, Map.add str.[0] (insert str.[1..] nextDict) r) *)

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


(*
type Dict =
    | Leaf of bool
    | Node of bool * Map<char, Dict>

let empty () = Leaf false*)
