module internal ActionGenerator
open System.Collections.Generic
open Microsoft.FSharp.Collections
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open StateMonad
open madsSW2.State
open Parser
open FParsecLight.TextParser
open System.Threading
        
type internal move = state -> (coord * (uint32 * (char * int))) list

let getRes sq = 
    match sq with
    |Success a -> a
    |Failure x -> x

let isHole (coord:coord)(board:board) = //(board.squares coord) |> getRes |> Option.isNone
   match (board.squares coord)  with
    | Success opt -> 
        let res = opt |> Option.isNone 
        ScrabbleUtil.DebugPrint.forcePrint $"%A{coord} -> %b{res}"
        res
    | Failure _ -> 
        ScrabbleUtil.DebugPrint.forcePrint $"We have a Failure"
        true 
// We need implementation from assignment 2.17 and 3.8 i think
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
                                if   (isFreeAdjacentSquares x y isHorizontal placedTiles) |> Async.RunSynchronously
                                then aux (MultiSet.removeSingle letterIndex hand) dic (placement::move) acc tiles (incrementCoord isHorizontal (x, y) )
                                else acc
                            | Some(true, dic) -> 
                                if (isFreeAdjacentSquares x y isHorizontal placedTiles) |> Async.RunSynchronously
                                then
                                    aux (MultiSet.removeSingle letterIndex hand) dic (placement::move) ((placement::move)::acc) tiles (incrementCoord isHorizontal (x, y))
                                //DebugPrint.forcePrint $"Found Word (128): %A{placement::move}\n" 
                                else acc
                                //ScrabbleUtil.DebugPrint.forcePrint $"Some(true)(126): %A{}\n"      
                ) acc tile
        ) moveList hand 
    aux hand dict [] [] tiles startCoord
        

let rec findMove hand dict placedTiles tiles (prefixList:List<coord * list<(int * int) * (uint * (char * int))>>) isHorizontal moveList (board:board) token =
    async{
        try
            match prefixList with
            | [] -> return moveList
            | prefix::t -> 
                let move = findPlayableTiles hand (prefixDict dict prefix) placedTiles tiles isHorizontal (getStartCoord prefix isHorizontal) board
                match move with 
                | [] ->    
                    return! findMove hand dict placedTiles tiles t isHorizontal moveList board token
                | _ -> 
                    //DebugPrint.forcePrint $"Valid move found:  %A{move}\n"
                    return! findMove hand dict placedTiles tiles t isHorizontal (moveList@move) board token
                
        with| :? System.OperationCanceledException -> return moveList
        }

let bestMove moveList = List.fold (fun acc elem -> if List.length elem > List.length acc then elem else acc) [] moveList

let collectMoveList st chunksVert chunksHori token= 
    async{
        let moveList =
            (List.fold (fun acc chunk -> 
                (findMove st.hand st.dict st.placedTiles st.tiles chunk true [] st.board token)::acc
            ) (List.fold (fun acc chunk -> 
                (findMove st.hand st.dict st.placedTiles st.tiles chunk false [] st.board token)::acc
            ) [] chunksVert) chunksHori
                |> Async.Parallel
                |> Async.RunSynchronously)
        return Array.fold (fun acc x -> x@acc) [] moveList
}



let generateAction (st:state) = 
        if Map.isEmpty st.placedTiles
        then //place first move center on empty board.
            let centerMoveList = findPlayableTiles st.hand st.dict Map.empty st.tiles true (0,0) st.board
            let centerMove = bestMove centerMoveList
            match centerMove.Length with
            |0 ->  [] //pass
            |_ ->  List.rev centerMove //play move
        else //find move
            let verticalPrefixList = Map.toList st.verticalPrefixes
            let horizontalPrefixList = Map.toList st.horizontalPrefixes
            
            DebugPrint.forcePrint $"Chunking vert\n"
            let maxChunkSize = 5
            let chunksVert = 
                match List.length verticalPrefixList >= maxChunkSize with
                | false -> [verticalPrefixList]
                | true -> List.chunkBySize (verticalPrefixList.Length/maxChunkSize) verticalPrefixList  
            
            DebugPrint.forcePrint $"Chunking hori\n"
            let chunksHori = 
                match List.length horizontalPrefixList >= maxChunkSize with
                | false -> [horizontalPrefixList]
                | true -> List.chunkBySize (horizontalPrefixList.Length/maxChunkSize) horizontalPrefixList
            
            DebugPrint.forcePrint $"Done Chunking\n"

            use cancelationToken = new CancellationTokenSource(1000)
            let token = cancelationToken.Token
            let mutable collectedMoveList = []

            //collectedMoveList <- (Async.StartAsTask((collectMoveList st chunksVert chunksHori token), Tasks.TaskCreationOptions.None, token) |> Async.AwaitTask |> Async.RunSynchronously)
            //let bestMove = bestMove (collectedMoveList)
            //DebugPrint.forcePrint $"Async bestMove (not reversed): %A{bestMove}\n"
            //List.rev bestMove
            try
                collectedMoveList <- (Async.StartAsTask((collectMoveList st chunksVert chunksHori token), Tasks.TaskCreationOptions.None, token) |> Async.AwaitTask |> Async.RunSynchronously)
                let bestMove = bestMove (collectedMoveList)
            //DebugPrint.forcePrint $"Async bestMove (not reversed): %A{bestMove}\n"
                List.rev bestMove
            with
            | :? System.OperationCanceledException -> 
                let bestMove = bestMove (collectedMoveList)
                DebugPrint.forcePrint $"Timeout reached, best move: %A{bestMove}\n"
                List.rev bestMove
            
            //DebugPrint.forcePrint $"Async moveList: %A{moveList}\n"
        
        
            
        


