namespace madsSW2

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open ActionGenerator
open System.IO
open State

open ScrabbleUtil.DebugPrint
// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        debugPrint("parsing move\n")
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |>
           
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList
    let parseChangePieces ts =
        debugPrint("parsing move\n")
        let pattern = @"([\d]){1,2}" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |>
           
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [id;] ->
                    (id |> uint32)
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint $"%d{x} -> (%A{Map.find x pieces}, %d{i})\n") ()
    
    let printPrefixes (prefixes: Map<coord,((int * int) * (uint * (char * int))) list>) = 
        prefixes |>
        Map.fold (fun _ x i -> 
                forcePrint(sprintf $"%A{x}")
                List.fold (fun _ ((_,_),(_,(c,_))) -> forcePrint(sprintf $"%c{c}" )) () i
                forcePrint $"\n"
                ) () 

module ManualPlay =
    let rec appendTiles n x acc = 
        match n with
        |0u -> acc
        |_ ->
            appendTiles (n-1u) x (x::acc)
    let handToIdList hand = hand |> MultiSet.fold (fun acc x n -> appendTiles n x acc ) []
    let parsePlayerAction cstream st (input:string) =
        let action = input.Substring(0, 2)
        match action with
        | "mo" ->
            let command = (input.Substring 3)
            let move = RegEx.parseMove command
            debugPrint $"Player %d{playerNumber st} -> Server:\n%A{move}\n"
            send cstream (SMPlay move)
        |"ch" ->
            debugPrint "Player wants to change pieces\n"
            let piecesToChange = handToIdList st.hand
            debugPrint $"Player %d{playerNumber st} <- Server:\n%A{piecesToChange}\n"
            send cstream (SMChange piecesToChange)
        | "ps" ->
            debugPrint "Player wants to pass\n"
            send cstream SMPass        
        | "ff" ->
            debugPrint "Player wants to ff\n"
            send cstream SMForfeit
        | _ -> failwith "Non valid command"

module Scrabble =
    open System.Threading
    
    let playGame cstream pieces (st : state) =

        let rec aux (st : state) =
            (*print hand
            Print.printHand pieces (hand st)*) 
            
            (* let input = System.Console.ReadLine()
            //For playing manually:
            let move = RegEx.parseMove (input)
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)
            send cstream (SMPlay move) *)

            //for Playing with bot
            match (st.playerTurn = st.playerNumber ) with
            | true -> 
                use cancellationSource = new CancellationTokenSource()
                let move = generateAction st
                match List.length move with
                | 0 ->
                    match st.allowChange with
                    |true -> send cstream (SMChange (ManualPlay.handToIdList st.hand))
                    //send cstream SMPass
                    |false -> send cstream SMPass
                | _ -> send cstream (SMPlay move )
            | false -> ()
            
            let msg = recv cstream 
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                debugPrint("succesful play by you!\n")    
                let st' = updateStatePlaySuccess st ms points newPieces
                aux st'
                
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                debugPrint("succesful play by other!\n")        
                let st' = updateStatePlayed st pid ms points
                aux st'
                
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                debugPrint $"failed play by %d{pid}. Attempted move: %A{ms} !\n"
                let st' = updateStatePlayerPassed st
                aux st'
            | RCM (CMPassed playerId) ->
                debugPrint($"Player %d{playerId} passed\n")
                let st' = updateStatePlayerPassed st
                aux st'
            | RCM (CMForfeit playerId) ->
                debugPrint($"Player %d{playerId} forfeited\n")
                let st' = updateStatePlayerForfeit st playerId
                aux st'
            | RCM(CMChangeSuccess(newPieces)) ->
                debugPrint("You changed pieces\n")
                let st' = updateStatePiecesChangedSuccess st newPieces
                aux st'
            | RCM(CMChange(playerId, numberOfTiles)) ->
                debugPrint($"Player %d{playerId} changed %d{numberOfTiles} pieces\n")              
                let st' = updateStatePlayerPassed st
                aux st'
            | RCM (CMGameOver finalSCore) ->
                debugPrint("Game over Final score:\n")
                finalSCore |>
                List.fold (fun _ (id,score) -> debugPrint $"Player: %d{id} -- Score: %d{score}\n") ()
            | RCM (CMTimeout time) -> 
                debugPrint($"Timeout %d{time}")
                let st' = updateStatePlayerPassed st
                aux st'
            | RGPE err ->
                debugPrint $"Gameplay Error:\n%A{err}"
                let mutable allowChange = true
                List.fold (fun _ error ->
                    match  error  with
                    | GPENotEnoughPieces _ ->
                        allowChange <- false
                        allowChange
                    | _ -> allowChange 
                    ) true err |> ignore
                match allowChange with
                | true ->
                    let st' = updateStatePlayerPassed st
                    aux st'
                | false ->
                    let st' = updateStateNotEnoughPieces st
                    aux st'
                    
        aux st
    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            $"Starting game!
                      number of players = %d{numPlayers}
                      player id = %d{playerNumber}
                      player turn = %d{playerTurn}
                      hand =  %A{hand}
                      timeout = %A{timeout}\n\n"
            
        
        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
        let forfeitedPlayers = List.Empty
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
        
        let boardTiles = Map.empty
        
        fun () -> playGame cstream tiles (mkState board dict numPlayers playerNumber playerTurn  handSet forfeitedPlayers boardTiles Map.empty Map.empty tiles timeout true)


        
                
       
            
