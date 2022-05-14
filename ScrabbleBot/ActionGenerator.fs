module internal ActionGenerator
open System.Collections.Generic
open Microsoft.FSharp.Collections
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open StateMonad
open madsSW2.State
open Parser
        
type internal move = state -> (coord * (uint32 * (char * int))) list


// We need implementation from assignment 2.17 and 3.8 i think
let isValid (coord:coord)(board:board) =
        let res = board.squares coord
        match res with
        | Success boardPos  ->
            match boardPos with  
            |None -> 
                ScrabbleUtil.DebugPrint.forcePrint $"We have a hole"
                false
            |Some squareProg ->
                let lookup = (squareProg.Item 0) Eval.hello 0 0
                ScrabbleUtil.DebugPrint.forcePrint $"values: %A{lookup}"
                true
        | Failure _ ->
            ScrabbleUtil.DebugPrint.forcePrint $"We have a hole"
            false
(* let isHole2 board coord index acc =
    board.squares coord |>
    fun(StateMonad.Success sq) -> sq |>
    Option.map (fun x -> Map.find i x Eval.) *)
    
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

        
let isFreeAdjacentSquares x y isHorizontal placedTiles : Async<bool> =
   async {
    let north = Map.containsKey (x,y-1) placedTiles //log n running time
    let south = Map.containsKey (x,y+1) placedTiles
    let east = Map.containsKey (x+1,y) placedTiles
    let west = Map.containsKey (x-1,y) placedTiles

    match isHorizontal with
    |true -> match north, east, south with
                |false, false, false -> return true
                |_,_,_ ->   return false
    |false -> match south, east, west with
                |false, false, false -> return true
                |_,_,_ ->  return false
   }
    




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


let findPlayableTiles (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (placedTiles:Map<coord,uint*(char*int)>) (tiles:Map<uint,tile>) (isHorizontal:bool) (startCoord:coord) (board:board) =
    
    //Step with word/chars already on the board as prefix.
    //Find word from tiles in hand
    let rec aux (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (move:((coord * (uint32 * (char * int))) list)) (moveList:((coord * (uint32 * (char * int))) list)list) (tiles:Map<uint,tile>) (x,y) = 
        //Translate hand to tiles
        MultiSet.fold(
            fun acc letterIndex _  ->
            
                let tile = Map.find letterIndex tiles //
                //Fold to check all if wildcard instance. Otherwise this will only run once
                //Move should prob be reversed
                Set.fold (
                    fun _ elem -> 
                        match isEmpty placedTiles (x,y) with
                        |false -> acc
                        |true ->
                            let placement = ((x,y),(letterIndex, elem))
                            match step (fst elem) dict with      
                            | None -> acc //No move
                            | Some(false, dic) -> 
                                if   (isFreeAdjacentSquares x y isHorizontal placedTiles) |> Async.RunSynchronously && isValid (coord(x,y)) board
                                then aux (MultiSet.removeSingle letterIndex hand) dic (placement::move) acc tiles (incrementCoord isHorizontal (x, y) )
                                else acc
                            | Some(true, _) -> 
                                if (isFreeAdjacentSquares x y isHorizontal placedTiles) |> Async.RunSynchronously && isValid (coord(x,y)) board
                                then
                                //DebugPrint.forcePrint $"Found Word (128): %A{placement::move}\n" 
                                (placement::move)::acc
                                else acc 
                                //ScrabbleUtil.DebugPrint.forcePrint $"Some(true)(126): %A{}\n"      
                ) acc tile
        ) moveList hand 
    aux hand dict [] [[]] tiles startCoord
        
        
    
    


let rec findMove hand dict placedTiles tiles (prefixList:List<coord * list<(int * int) * (uint * (char * int))>>) isHorizontal moveList (board:board) =
    match prefixList with
    | [] -> moveList
    | prefix::t -> 
        let move = findPlayableTiles hand (prefixDict dict prefix) placedTiles tiles isHorizontal (getStartCoord prefix isHorizontal) board
        match move with 
        | [] -> findMove hand dict placedTiles tiles t isHorizontal moveList board
        | _ -> findMove hand dict placedTiles tiles t isHorizontal (moveList@move) board

let bestMove moveList = List.fold (fun acc elem -> if List.length elem > List.length acc then elem else acc) [] moveList

let generateAction (st:state) = 
    
    if Map.isEmpty st.placedTiles
    then //place first move center on empty board.
        let centerMoveList = findPlayableTiles st.hand st.dict Map.empty st.tiles true (0,0) st.board
        let centerMove = bestMove centerMoveList
        match centerMove.Length with
        |0 -> [] //pass
        |_ ->  List.rev centerMove //play move
    else //find move
        let verticalPrefixList = Map.toList st.verticalPrefixes
        let horizontalPrefixList = Map.toList st.horizontalPrefixes

        let verticalMoveList = findMove st.hand st.dict st.placedTiles st.tiles verticalPrefixList false [] st.board
        let verticalMove = bestMove verticalMoveList 

        let horizontalMoveList = findMove st.hand st.dict st.placedTiles st.tiles horizontalPrefixList true [] st.board
        let horizontalMove = bestMove horizontalMoveList

        match verticalMove.Length, horizontalMove.Length  with 
        |0,0 -> []//change pieces?
        |0,_ -> List.rev horizontalMove 
        |_,0 -> List.rev verticalMove
        |x,y -> if x >=y then verticalMove else horizontalMove
        


