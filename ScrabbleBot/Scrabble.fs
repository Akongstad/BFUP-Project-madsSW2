namespace madsSW2

open System.Security.AccessControl
open MultiSet
open ScrabbleUtil
open ScrabbleUtil.Dictionary
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open StateMonad
open madsSW2

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
        boardTiles    : Map<coord,(char*int)>
        //number of players (så vi bl.a ved at hvis en forfeiter, og de kun er 2, så har den anden vundet.
        //player turn
        //hvordan holdes der styr på point? - det gør serveren.
    }

    let mkState b d np pn pt h g bT = {board = b; dict = d; numberofPlayers = np; playerNumber = pn; ForfeitedPlayers =g; playerTurn = pt; hand = h; boardTiles = bT}

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playerTurn st = st.playerTurn
    
    let numberOfPlayers st = st.numberofPlayers
    let ForfeitedPlayers st = st.ForfeitedPlayers
    
    let boardtiles st = st.boardTiles
    
    // move: ((int * int) * (uint32 * (char * int))) list
    //helper methods
     
    let getCoordAndTileFromMove moveList boardTiles = List.fold (fun acc (x,k) -> Map.add (coord(x)) (snd(k)) acc) boardTiles moveList //get list of coord and (char * char point val)
    let getUsedTileIdFromMove moveList = List.fold (fun acc (x,k) -> fst(k)::acc) List.Empty moveList //get the tile ids of the played move
    
    let removeFromHandSet playedPieces hand = List.fold (fun acc x -> MultiSet.remove x 1u acc) hand playedPieces //removes the played tiles from the hand
    
    let addToHandSet newTiles hand = List.fold (fun acc (x, k) -> MultiSet.add x k acc) hand newTiles //adds new tiles to the hand
    
    let changePlayerTurn (st:state) =
                let rec player pnr =
                    if st.numberofPlayers = pnr then
                        if List.contains 1u st.ForfeitedPlayers then
                            player 1u
                        else 1u
                    else if (List.contains (pnr+1u ) st.ForfeitedPlayers) then

                            player st.playerTurn +  1u
                         else st.playerTurn +  1u
                player st.playerNumber
    let updatedForfeitedPlayers (st : state) playerId = playerId::st.ForfeitedPlayers
    
    
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
        
        
        
            
    // '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )
    (*HEURISTIC*)
    let bestMove (st : state) =
        let board = st.board
        let hand = st.hand
        let dict = st.dict
        let boardTiles = st.boardTiles
        
        
        let coords = List.ofSeq boardTiles.Keys
        
        let auxCheckedCoords = List.empty
        
        
        let rec aux (c:coord) (v:(char*int)) =
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
                    let bka = step 'c' dict
                    
            
            
            
        aux coords.Head boardTiles[coords.Head]
        
    let rec traverseTrie (st:state) (hand:MultiSet.MultiSet<uint32>) (dict:Dict) (cList:List<char>) (c:char) =  
        MultiSet. ::cList
        
        let rec stepNextChar (c:char)
            match step c dict with
            |None -> None
            
            
        
        

module Scrabble =
    open System.Threading
    

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)
            

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input = System.Console.ReadLine()
            let action = input.Substring(0, 2)
            let command = (input.Substring 3)
            match action with
            | "mo" ->
                let move = RegEx.parseMove (command)
                debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)
                send cstream (SMPlay move)
            |"ch" ->
                let piecesToChange = RegEx.parseChangePieces command
                //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move)
                send cstream (SMChange piecesToChange)
            | _ -> failwith "todo"  (*vi skal have lavet en funktion lige her, som efter en eller anden heuristik kan finde det næste move*)
                            
            (*let move = RegEx.parseMove (command)
            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move)
            send cstream (SMPlay move)*)
            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            
            
            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                printf("succesful play by you!")
                
                forcePrint $"Variables:!
                      ms = %A{ms}
                      points = %d{points}
                      newPieces = %A{newPieces}\n\n"
                let usedTileIds = State.getUsedTileIdFromMove move
                let currentHand = State.removeFromHandSet usedTileIds st.hand  
                let nextHand = State.addToHandSet newPieces currentHand
                
                let newBoardTiles = State.getCoordAndTileFromMove ms st.boardTiles
                
                let st' = State.mkState st.board st.dict st.numberofPlayers st.playerNumber (State.changePlayerTurn st) nextHand st.ForfeitedPlayers newBoardTiles
                aux st'

                
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                printf("succesful play by other!")
                
                let newBoardTiles = State.getCoordAndTileFromMove ms st.boardTiles
                
                let st' = State.mkState st.board st.dict st.numberofPlayers st.playerNumber (State.changePlayerTurn st) st.hand st.ForfeitedPlayers newBoardTiles
                aux st'
                
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                printf("failed play by you!")
                
                let st' = State.mkState st.board st.dict st.numberofPlayers st.playerNumber (State.changePlayerTurn st) st.hand st.ForfeitedPlayers st.boardTiles 
                aux st'
                
            | RCM (CMForfeit playerId) ->
                printf("Player {pid} forfeited")
                let updatedForfeitedPlayers (st : State.state) = List.insertAt 1 playerId st.ForfeitedPlayers
                let st' = State.mkState st.board st.dict st.numberofPlayers  st.playerNumber  (State.changePlayerTurn st)   st.hand (updatedForfeitedPlayers st) st.boardTiles
                aux st'
            | RCM(CMChangeSuccess(newPieces)) ->
                printf("You changed pieces")
                let currentHand = State.removeFromHandSet (RegEx.parseChangePieces command) st.hand  
                let nextHand = State.addToHandSet newPieces st.hand
                let st' = State.mkState st.board st.dict st.numberofPlayers  st.playerNumber  (State.changePlayerTurn st) nextHand st.ForfeitedPlayers st.boardTiles
                aux st'
            | RCM(CMChange(playerId, numberOfTiles)) ->
                printf($"Player %d{playerId} changed %d{numberOfTiles} pieces")              
                let st' = State.mkState st.board st.dict st.numberofPlayers  st.playerNumber  (State.changePlayerTurn st) st.hand st.ForfeitedPlayers st.boardTiles
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                let st' = State.mkState st.board st.dict st.numberofPlayers st.playerNumber (State.changePlayerTurn st) st.hand st.ForfeitedPlayers st.boardTiles
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
        
        
        
        fun () -> playGame cstream tiles (State.mkState board dict numPlayers playerNumber playerTurn  handSet forfeitedPlayers boardTiles)


            
            
            //((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
        
        (*let spiral (X:int) (Y:int) =
            let x,y = 0
            let dx = 0
            let dy = -1
            let maxSquared = sqrt(max(X,Y))
            let counter = 0
            
            let rec aux x y X Y count=
                if (-X/2 < x <= X/2) && (-Y/2 < y <= Y/2)
                then printf(x,y)
                if x == y && (x < 0 && x == -y) && (x > 0 && x == 1-y)
                then
                    dx, dy = -dy, dx
                
                x, y = x+dx, y+dy    
           
        aux x y X Y  *)      
                
                
       
            
