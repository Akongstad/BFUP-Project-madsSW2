module Dictionary

type Dictionary =
    | Leaf of bool
    | Node of bool * Map<char, Dictionary>



let empty () = Leaf false

let rec insert s =
    function
    | Leaf _ when String.length s <= 0 -> Leaf true
    | Node (_, map) when String.length s <= 0 -> Node(true, map)
    | Leaf b when s.Length > 0 -> Node(b, Map.empty.Add(s.Chars 0, insert (s.Remove(0, 1)) (empty ())))
    | Node (b, map) when map.ContainsKey(s.Chars 0) ->
        Node(b, map.Add(s.Chars 0, insert (s.Remove(0, 1)) (map.Item(s.Chars 0))))
    | Node (b, map) -> Node(b, map.Add(s.Chars 0, insert (s.Remove(0, 1)) (empty ())))

let rec lookup s =
    function
    | Leaf b when String.length s = 0 -> b
    | Leaf _ -> false
    | Node (b, _) when s.Length = 0 -> b
    | Node (_, map) when map.ContainsKey(s.Chars 0) -> lookup (s.Remove(0, 1)) (map.Item(s.Chars 0))
    | Node (_, map) when not (map.ContainsKey(s.Chars 0)) -> false

let isTrue =
    function
    | Leaf b -> b
    | Node (b, _) -> b

let rec step c =
    function
    | Leaf _ -> None
    | Node (_, map) when not (map.ContainsKey c) -> None
    | Node (b, map) when isTrue (map.Item c) -> Some(true, map.Item c)
    | Node (b, map) -> Some(false, map.Item c)
