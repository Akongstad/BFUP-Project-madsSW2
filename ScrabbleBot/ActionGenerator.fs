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

(* type directionClear =
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
            |_ -> false *)
        
(* let getDirectionClear (c:coord) (st:state) :directionClear =
    let topClear =  Map.containsKey (fst(c),snd(c)+1) st.placedTiles
    let leftClear = Map.containsKey (fst(c)+1,snd(c)) st.placedTiles
    match topClear,leftClear with
    |true, true -> Both
    |false, false -> Neither
    |true, false -> Top
    |false, true  -> Left
     *)
    
let prefixDict (dict:Dict) (prefixChars:coord * list<(int * int) * (uint * (char * int))>) = 
    
    List.fold (
        fun acc ((_,_), (_, (c,_))) -> //(firstCharCoord:coord) (letterCoord:(int*int)) ((letterId:uint), c) 
            match (Dictionary.step c acc) with
            | None -> failwith "Invalid boardChars" //Should never
            | Some(_, dic) -> dic
       ) dict (snd prefixChars)

(* let rec suffixCheck (dict:Dict) (suffixChars:Map<coord,((int * int) * (uint * (char * int))) list>) (move:List<uint*(char*int)>) =
    Map.fold (
        fun acc (a:coord) b  ->
         //TODO add uint id
         let placement = 0u,b
         match (Dictionary.step (fst b) dict) with
         | None -> []
         | Some(false, dic) -> suffixCheck dic ((Map.remove a) suffixChars) (placement::move)
         | Some(true, _) -> (placement::move)
       ) [] suffixChars *)


let incrementCoord isHorizontal (x,y) = 
    match isHorizontal with
    | true -> x+1, y
    | false -> x, y+1

let getCoord ((x,y), (ui,(c,i)))= x,y

let getStartCoord (prefixChars:coord * list<(int * int) * (uint * (char * int))>) isHorizontal = 
    prefixChars 
    |> snd 
    |> List.last 
    |> getCoord
    |> incrementCoord isHorizontal


let findPlayableTiles (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (placedTiles:Map<coord,uint*(char*int)>) (tiles:Map<uint,tile>) (isHorizontal:bool) (startCoord:coord) =
    //Step with word/chars already on the board as prefix.
    
    //Find word from tiles in hand
    let rec aux (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (move:(coord * (uint32 * (char * int))) list) (tiles:Map<uint,tile>) (x,y) = 
        //Translate hand to tiles
        MultiSet.fold(
            fun acc letterIndex _  ->
                let tile = Map.find letterIndex tiles //
                //Fold to check all if wildcard instance. Otherwise this will only run once
                Set.fold (
                    fun acc' elem -> 
                        let placement = ((x,y),(letterIndex, elem))
                        match step (fst elem) dict, (isEmpty placedTiles (x,y)) with
                        | _, false -> []
                        | None, _ -> [] //No move
                        | Some(false, dic), _ -> aux (MultiSet.removeSingle letterIndex hand) dic (placement::acc') tiles (incrementCoord isHorizontal (x, y) )
                        | Some(true, _), _ -> (placement::acc')
                ) acc tile
        )move hand
    aux hand dict [] tiles startCoord


let rec findMove hand dict placedTiles tiles (prefixList:List<coord * list<(int * int) * (uint * (char * int))>>) isHorizontal =
    match prefixList with
    | [] -> []
    | prefix::t -> 
        let move = findPlayableTiles hand (prefixDict dict prefix) placedTiles tiles isHorizontal (getStartCoord prefix isHorizontal)
        match move with 
        | [] -> findMove hand dict placedTiles tiles t isHorizontal
        | _ -> move



let generateAction (st:state) = 
    //if Map.isEmpty st.placedTiles then findPlayableTiles st.hand startCoord (0,0) true else 
    let verticalPrefixList = Map.toList st.verticalPrefixes
    let horizontalPrefixList = Map.toList st.horizontalPrefixes

    let verticalMove = findMove st.hand st.dict st.placedTiles st.tiles verticalPrefixList false
    let horizontalMove = findMove st.hand st.dict st.placedTiles st.tiles horizontalPrefixList true 

    match verticalMove.Length, horizontalMove.Length  with 
    |0,0 -> []//change pieces?
    |0,_ -> horizontalMove
    |_,0 -> verticalMove
    |x,y -> if x >=y then verticalMove else horizontalMove

    //if Map.isEmpty st.placedTiles then findPlayableTiles st.hand startCoord (0,0) true else 
