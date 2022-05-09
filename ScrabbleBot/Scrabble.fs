namespace madsSW2

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open ActionGenerator
open System.IO
open State

open ScrabbleUtil.DebugPrint
open StateMonad
// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        printf("parsing move\n")
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
        printf("parsing move\n")
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
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()
    
    let printPrefixes (prefixes: Map<coord,((int * int) * (uint * (char * int))) list>) = 
        prefixes |>
        Map.fold (fun _ x i -> forcePrint (sprintf "%A -> (%A)\n" x i)) ()

module ManualPlay =
    let handToIdList hand = hand |> MultiSet.fold (fun acc x _ -> x::acc ) []
    let parsePlayerAction cstream st (input:string) =
        let action = input.Substring(0, 2)
        match action with
        | "mo" ->
            forcePrint "Player wants to move\n"
            let command = (input.Substring 3)
            let move = RegEx.parseMove (command)
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)
            send cstream (SMPlay move)
        |"ch" ->
            forcePrint "Player wants to change pieces\n"
            let piecesToChange = handToIdList st.hand
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) piecesToChange)
            send cstream (SMChange piecesToChange)
        | "ps" ->
            forcePrint "Player wants to pass\n"
            send cstream SMPass        
        | "ff" ->
            forcePrint "Player wants to ff\n"
            send cstream SMForfeit
        | _ -> failwith "Non valid command"

module Scrabble =
    open System.Threading
    
    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            
            forcePrint $"Horizontal:  "
            Print.printPrefixes st.horizontalPrefixes
            
            forcePrint $"Vertical:  "
            Print.printPrefixes st.verticalPrefixes
            
            forcePrint $"Turn = %d{st.playerTurn} %d{st.playerNumber}\n0 "
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            
            let input = System.Console.ReadLine()
            //For playing manually:
            let move = RegEx.parseMove (input)
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)
            send cstream (SMPlay move)

            //FOR Playing with bot
           (*  match (st.playerTurn = st.playerNumber ) with
            | true -> 
                let move = generateAction st
                match List.length move with
                | 0 -> send cstream SMChange
                | _ -> send cstream (SMPlay move )
            | _ -> failwith "How do we wait?" *)
            
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move)
            
            let msg = recv cstream 
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                forcePrint( sprintf "ms: %A \n" ms)
                printf("succesful play by you!\n")    
                let st' = updateStatePlaySuccess st ms points newPieces
                aux st'

            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                printf("succesful play by other!\n")        
                let st' = updateStatePlayed st pid ms points
                aux st'
                
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                printf("failed play by you!\n")
                let st' = updateStatePlayerPassed st
                aux st'
            | RCM (CMPassed playerId) ->
                printf($"Player %d{playerId} passed\n")
                let st' = updateStatePlayerPassed st
                aux st'
            | RCM (CMForfeit playerId) ->
                printf($"Player %d{playerId} forfeited\n")
                let st' = updateStatePlayerForfeit st playerId
                aux st'
            | RCM(CMChangeSuccess(newPieces)) ->
                printf("You changed pieces\n")
                let st' = updateStatePiecesChangedSuccess st newPieces
                aux st'
            | RCM(CMChange(playerId, numberOfTiles)) ->
                printf($"Player %d{playerId} changed %d{numberOfTiles} pieces\n")              
                let st' = updateStatePlayerPassed st
                aux st'
            | RCM (CMGameOver finalSCore) ->
                printf("Game over Final score:")
                finalSCore |>
                List.fold (fun _ (id,score) -> printf($"Player: %d{id} -- Score: %d{score}\n")) ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                let st' = updateStatePlayerPassed st
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
        forcePrint 
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
        //TODO Save this in state. For checking whether a square is a hole and calculating points 
        let boardFun =
            Map.map(fun _ value -> Parser.parseSquareProg value) boardP.squares
            |> Parser.parseBoardProg boardP.prog
        
        fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn  handSet forfeitedPlayers boardTiles Map.empty Map.empty tiles)


        
                
       
            
