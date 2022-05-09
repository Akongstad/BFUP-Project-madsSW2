module internal ActionGenerator
open System.Collections.Generic
open Microsoft.FSharp.Collections
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open madsSW2.State
open Parser
        
type internal move = state -> (coord * (uint32 * (char * int))) list


// Is square a hole? If square does not have a squareprog. eg doubleLetterScore. Square is hole.
// We need implementation from assignment 2.17 and 3.8 i think
let isHole (coord:coord) (board:board) (squares:Map<int, squareProg>) = failwith "Not implemented"
    
let isEmpty (placedTiles: Map<coord,uint*(char*int)>) (coord:coord) =
     not(Map.containsKey (fst(coord),snd(coord)) placedTiles)

let isValidMove (move: (coord * (uint32 * (char * int))) list) (placedTiles: Map<coord,uint*(char*int)>) =
        move
        |> List.fold (fun acc x -> (isEmpty placedTiles (fst x)::acc)) []
        |> List.contains false

type directionClear =
        |Top
        |Left
        |Both
        |Neither

let directionClear (c:coord) (st:state) direction =
    match direction with
    | Top -> match Map.tryFind (fst(c),snd(c)+1) st.placedTiles with
            |None -> true
            |_ -> false
    | Left -> match Map.tryFind (fst(c)-1,snd(c)) st.placedTiles with
            |None -> true
            |_ -> false
        
let getDirectionClear (c:coord) (st:state) :directionClear =
    let topClear =  Map.containsKey (fst(c),snd(c)+1) st.placedTiles
    let leftClear = Map.containsKey (fst(c)+1,snd(c)) st.placedTiles
    match topClear,leftClear with
    |true, true -> Both
    |false, false -> Neither
    |true, false -> Top
    |false, true  -> Left
    
    
let prefixDict (dict:Dict) (prefixChars:Map<coord,uint*(char*int)>) = 
    Map.fold (
        fun acc (a:coord) ((b:uint),c ) ->
         match (Dictionary.step (fst c) acc) with
         | None -> failwith "Invalid boardChars" //Should never
         | Some(_, dic) -> dic
       ) dict prefixChars

let rec suffixCheck (dict:Dict) (suffixChars:Map<coord,char*int>) (move:List<uint*(char*int)>) =
    Map.fold (
        fun acc (a:coord) b  ->
         //TODO add uint id
         let placement = 0u,b
         match (Dictionary.step (fst b) dict) with
         | None -> []
         | Some(false, dic) -> suffixCheck dic ((Map.remove a) suffixChars) (placement::move)
         | Some(true, _) -> (placement::move)
       ) [] suffixChars

let incrementCoord x y = 
    function
    | true -> x+1, y
    | false -> x, y+1

let getStartCoord x y (prefixChars:List<coord*uint*(char*int)>) isHorizontal = 
    fst(List.last prefixChars)


let findPlayableTiles (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (placedTiles:Map<coord,uint*(char*int)>) (tiles:List<tile*uint>) (isHorizontal:bool) (startCoord:coord) =
    //Step with word/chars already on the board as prefix.
    
    //Find word from tiles in hand
    let rec aux (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (move:(coord * (uint32 * (char * int))) list) (tiles:Map<uint32, tile>) (x,y) = 
        //Translate hand to tiles
        MultiSet.fold(
            fun acc letterIndex _  ->
            let tile = tiles.Item letterIndex
            //Fold to check all if wildcard instance. Otherwise this will only run once
            Set.fold (
                fun acc' elem -> 
                    let placement = ((x,y),(letterIndex, elem))
                    match step (fst elem) dict, (isEmpty placedTiles (x,y)) with
                    | _, false -> []
                    | None, _ -> [] //No move
                    | Some(false, dic), _ -> aux (MultiSet.removeSingle letterIndex hand) dic (placement::acc') tiles (incrementCoord x y isHorizontal)
                    | Some(true, _), _ -> (placement::acc')
                ) acc tile
            )move hand
    aux hand dict [] tiles startCoord


    
let generateAction (st:state) prefixChars:(coord * (uint32 * (char * int))) list = 
    //if Map.isEmpty st.placedTiles then findPlayableTiles st.hand startCoord (0,0) true else 
    let rec aux acc=
        Map.fold ( fun x coord tile ->
            match getDirectionClear coord st with
            |Both -> 
                let verticalWord = findPlayableTiles st.hand (prefixDict st.dict prefixChars) st.placedTiles (ScrabbleUtil.English.tiles 1u) false (getStartCoord fst(coord) snd(coord))
                let horizontalWord = findPlayableTiles st.hand (prefixDict st.dict prefixChars) st.placedTiles (ScrabbleUtil.English.tiles 1u) true getStartCoord
                match verticalWord.Length , horizontalWord.Length with
                |0,0 -> []//change pieces?
                |0,_ -> horizontalWord
                |_,0 -> verticalWord
                |x,y -> if x >=y then verticalWord else horizontalWord 
            |Neither -> []
            |Top ->findPlayableTiles st.hand (prefixDict dict prefixChars)  st.placedTile false getStartCoord  
            |Left -> findPlayableTiles st.hand (prefixDict dict prefixChars) st.placedTiles true getStartCoord
                
        ) [] st.placedWords

        
    if Map.isEmpty st.placedTiles then findPlayableTiles st.hand startCoord (0,0) true else 
    aux []

// '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )
    (*HEURISTIC*)
(*let bestMove (st : state) =
    let board = st.board
    let hand = st.hand
    let dict = st.dict
    let placedTiles = st.placedTiles
    
    
    let coords = List.ofSeq placedTiles.Keys
    
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
                aux coords.Head placedTiles[coords.Head]
            |Both -> dunno
            |Top ->
                    let bka = step 'c' dict*)
