module internal ActionGenerator
open System.Collections.Generic
open Microsoft.FSharp.Collections
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open madsSW2.State
        
type internal move = state -> (coord * (uint32 * (char * int))) list
let generateAction (st:state) :(coord * (uint32 * (char * int))) list = failwith "not implemented"


// Is square a hole? If square does not have a squareprog. eg doubleLetterScore. Square is hole.
// We need implementation from assignment 2.17 and 3.8 i think
let isHole (coord:coord) (squares:Map<int, squareProg>) = failwith "Not implemented"
    
let isEmpty (boardTiles: Map<coord, char* int>) (coord:coord) =
     not(Map.containsKey (fst(coord),snd(coord)) boardTiles)

let isValidMove (move: (coord * (uint32 * (char * int))) list) (boardTiles: Map<coord, char* int>) =
        move
        |> List.fold (fun acc x -> (isEmpty boardTiles (fst x)::acc)) []
        |> List.contains false

type directionClear =
        |Top
        |Left
        |Both
        |Neither

let directionClear (c:coord) (st:state) direction =
    match direction with
    | Top -> match Map.tryFind (fst(c),snd(c)+1) st.boardTiles with
            |None -> true
            |_ -> false
    | Left -> match Map.tryFind (fst(c)-1,snd(c)) st.boardTiles with
            |None -> true
            |_ -> false
        
let getDirectionClear (c:coord) (st:state) :directionClear =
    let topClear =  Map.containsKey (fst(c),snd(c)+1) st.boardTiles
    let leftClear = Map.containsKey (fst(c)+1,snd(c)) st.boardTiles
    match topClear,leftClear with
    |true, true -> Both
    |false, false -> Neither
    |true, false -> Top
    |false, true  -> Left
    
    
let prefixDict (dict:Dict) (boardChars:Map<coord,char*int>) =
    Map.fold (
        fun acc (a:coord) b  ->
         match (Dictionary.step (fst b) acc) with
         | None -> failwith "Invalid boardChars" //Should never
         | Some(_, dic) -> dic
       ) dict boardChars
let rec suffixDict (dict:Dict) (boardChars:Map<coord,char*int>) (move:List<uint*(char*int)>) =
    Map.fold (
        fun acc (a:coord) b  ->
         //TODO add uint id
         let placement = 0u,b
         match (Dictionary.step (fst b) dict) with
         | None -> []
         | Some(false, dic) -> suffixDict dic ((Map.remove a) boardChars) (placement::move)
         | Some(true, _) -> (placement::move)
       ) [] boardChars
let rec findPlayableTiles (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (boardChars:Map<coord,char*int>) (tiles:Map<uint32, tile>) =
    //Step with word/chars already on the board as prefix.
    let newDict = prefixDict dict boardChars 
    
    //Find word from tiles in hand
    let rec aux (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (move:List<uint*(char*int)>) (tiles:Map<uint32, tile>) = 
        MultiSet.fold(
            fun acc letterIndex _  ->
            let tile = tiles.Item letterIndex
            Set.fold (
                fun acc' elem -> 
                    let placement = (letterIndex, elem)
                    match step (fst elem) dict with
                    | None -> [] //No move
                    | Some(false, dic) -> aux (MultiSet.removeSingle letterIndex hand) dic (placement::acc') tiles
                    | Some(true, _) -> (placement::acc')
                ) acc tile
            )move hand
    aux hand newDict [] tiles

// '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )
    (*HEURISTIC*)
(*let bestMove (st : state) =
    let board = st.board
    let hand = st.hand
    let dict = st.dict
    let boardTiles = st.boardTiles
    
    
    let coords = List.ofSeq boardTiles.Keys
    
    let auxCheckedCoords = List.empty*)

    (*let rec aux (c:coord) (v:(char*int)) =
        if List.contains c auxCheckedCoords
        then coords = coords.Tail
        else
        
        let dirClear = getDirectionClear c st
        match dirClear with
            |Neither ->
                c::auxCheckedCoords
                coords = coords.tail
                aux coords.Head boardTiles[coords.Head]
            |Both -> dunno
            |Top ->
                    let bka = step 'c' dict*)
