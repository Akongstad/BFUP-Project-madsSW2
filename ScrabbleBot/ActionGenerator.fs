module internal ActionGenerator
open Microsoft.FSharp.Collections
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open StateMonad
open madsSW2.State
open Parser
open System.Threading
        
let isHole (coord:coord)(board:board) =
   match (board.squares coord)  with
    | Success opt -> 
        let res = opt |> Option.isNone 
        res
    | Failure _ -> 
        true
    
let isEmpty (placedTiles: Map<coord,uint*(char*int)>) (coord:coord) =
     not(Map.containsKey (fst(coord),snd(coord)) placedTiles)
    
let prefixDict (dict:Dict) (prefixChars:coord * list<(int * int) * (uint * (char * int))>) = 
    List.fold (
        fun acc ((_,_), (_, (c,_))) ->
            match (step c acc) with
            | None -> failwith "Invalid boardChars" //Will only happen if state is not consistent
            | Some(_, dic) -> dic
       ) dict (snd prefixChars)
        
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

let getCoord ((x,y), (_,(_,_)))= x,y

let getStartCoord (prefixChars:coord * list<(int * int) * (uint * (char * int))>) isHorizontal = 
    prefixChars 
    |> snd 
    |> List.last 
    |> getCoord
    |> incrementCoord isHorizontal


let findPlayableTiles (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (placedTiles:Map<coord,uint*(char*int)>) (tiles:Map<uint,tile>) (isHorizontal:bool) (startCoord:coord) (_board:board) = 
    //Step with word/chars already on the board as prefix.
    //Find word from tiles in hand
    let rec aux (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (move:(coord * (uint32 * (char * int))) list) (moveList:(coord * (uint32 * (char * int))) list list) (tiles:Map<uint,tile>) (x,y) = 
        //Translate hand to tiles
        MultiSet.fold(
            fun acc letterIndex _  ->
                let tile = Map.find letterIndex tiles //
                //Fold to check all if wildcard instance. Otherwise this will only run once
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
                                else acc     
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
            // measure algorithm runtime:
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            let verticalPrefixList = Map.toList st.verticalPrefixes
            let horizontalPrefixList = Map.toList st.horizontalPrefixes
            
            //If 1 -> creates 1 process per prefix.
            //If 10 -> creates 1 process per 10 prefixes
            let maxChunkSize = 1
            let chunksVert = 
                match List.length verticalPrefixList >= maxChunkSize with
                | false -> [verticalPrefixList]
                | true -> List.chunkBySize maxChunkSize verticalPrefixList  
            
            let chunksHori = 
                match List.length horizontalPrefixList >= maxChunkSize with
                | false -> [horizontalPrefixList]
                | true -> List.chunkBySize maxChunkSize horizontalPrefixList
            
            let timeout =
                match st.timeout with
                |Some time -> int(time)
                |None -> 0
                
            use cancelationToken = new CancellationTokenSource(timeout)
            let token = cancelationToken.Token
            let mutable collectedMoveList = []
            try
                collectedMoveList <- (Async.StartAsTask((collectMoveList st chunksVert chunksHori token), Tasks.TaskCreationOptions.None, token) |> Async.AwaitTask |> Async.RunSynchronously)
                let bestMove = bestMove collectedMoveList

                stopWatch.Stop()
                DebugPrint.debugPrint $"%f{stopWatch.Elapsed.TotalMilliseconds}"
                List.rev bestMove
            with
            | :? System.OperationCanceledException -> 
                let bestMove = bestMove collectedMoveList
                DebugPrint.debugPrint $"Timeout reached, best move: %A{bestMove}\n"
                List.rev bestMove 