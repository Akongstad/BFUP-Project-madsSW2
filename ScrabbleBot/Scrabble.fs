﻿namespace madsSW2

open MultiSet
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

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

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board

        dict          : Dictionary.Dict
        numberofPlayers : uint32
        ForfeitedPlayers : List<uint32>
        playerNumber  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        //number of players (så vi bl.a ved at hvis en forfeiter, og de kun er 2, så har den anden vundet.
        //player turn
        //hvordan holdes der styr på point? - det gør serveren.
    }

    let mkState b d np pn pt h g = {board = b; dict = d; numberofPlayers = np; playerNumber = pn; ForfeitedPlayers =g; playerTurn = pt; hand = h }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playerTurn st = st.playerTurn
    
    let numberOfPlayers st = st.numberofPlayers
    let ForfeitedPlayers st = st.ForfeitedPlayers

module Scrabble =
    open System.Threading
    

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
            let move = RegEx.parseMove input (*vi skal have lavet en funktion lige her, som efter en eller anden heuristik kan finde det næste move*)
           
            let changePlayerTurn (st: State.state) =
                let rec player pnr =
                    if st.numberofPlayers = pnr then
                        if List.contains 1u st.ForfeitedPlayers then
                            player 1u
                        else 1u
                    else if (List.contains (pnr+1u ) st.ForfeitedPlayers) then
                            player st.playerTurn +  1u
                         else st.playerTurn +  1u
                player st.playerNumber
                            
       
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)

                printf("succesful play by you!")
                let state = State.mkState st.board st.dict st.numberofPlayers st.playerNumber   (changePlayerTurn st)  st.hand st.ForfeitedPlayers
                aux state 
                
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                printf("succesful play by other!")
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                printf("failed play by you!")
                let st' = st // This state needs to be updated
                aux st'
                //let state = State.mkState st.board st.dict st.numberofPlayers st.playerNumber   (changePlayerTurn st) st.ForfeitedPlayers st.hand
                //aux state 
            | RCM (CMForfeit playerId) ->
                printf("Player {pid} forfeited")
                let updatedForfeitedPlayers (st : State.state) = List.insertAt 1 playerId st.ForfeitedPlayers
                let state = State.mkState st.board st.dict st.numberofPlayers  st.playerNumber  (changePlayerTurn st)   st.hand (updatedForfeitedPlayers st)
                aux state
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st
           
                
        

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
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)
            
           
        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.mkBoard boardP
        let forfeitedPlayers = List.Empty
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn  handSet forfeitedPlayers)
        
        
        // '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )
        (*HEURISTIC*)
        //let bestMove (st : State.state)
           (* let board = st.board
            let squares = board.squares
            let hand = st.hand
            let dict = st.dict
            
            let rec aux (c:Coord) (x:int) (y:int) =
                
                
                
                
            
           
            
            
           aux(board.center)  *)          
            
            
            //((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))

