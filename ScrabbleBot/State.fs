namespace madsSW2
open Parser
open ScrabbleUtil
module internal State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : board
        dict          : Dictionary.Dict
        numberOfPlayers : uint32
        ForfeitedPlayers : List<uint32>
        playerNumber  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        boardTiles    : Map<coord,(char*int)>
        //number of players (så vi bl.a ved at hvis en forfeiter, og de kun er 2, så har den anden vundet.
        //player turn
        //hvordan holdes der styr på point? - det gør serveren.
    }

    let mkState b d np pn pt h g bT = {board = b; dict = d; numberOfPlayers = np; playerNumber = pn; ForfeitedPlayers =g; playerTurn = pt; hand = h; boardTiles = bT}
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playerTurn st = st.playerTurn
    let numberOfPlayers st = st.numberOfPlayers
    let ForfeitedPlayers st = st.ForfeitedPlayers
    let boardTiles st = st.boardTiles
    
    // move: ((int * int) * (uint32 * (char * int))) list
    //helper methods
     
        
    let getCoordAndTileFromMove moveList boardTiles = List.fold (fun acc (x,k) -> Map.add (coord(x)) (snd(k)) acc) boardTiles moveList //get list of coord and (char * char point val)
    let getUsedTileIdFromMove moveList = List.fold (fun acc (x,k) -> fst(k)::acc) List.Empty moveList //get the tile ids of the played move
    
    let removeFromHandSet playedPieces hand = List.fold (fun acc x -> MultiSet.remove x 1u acc) hand playedPieces //removes the played tiles from the hand
    
    let addToHandSet newTiles hand = List.fold (fun acc (x, k) -> MultiSet.add x k acc) hand newTiles //adds new tiles to the hand
    
    let changePlayerTurn (st:state) =
                let rec player pnr =
                    if st.numberOfPlayers = pnr then
                        if List.contains 1u st.ForfeitedPlayers then
                            player 1u
                        else 1u
                    else if (List.contains (pnr+1u ) st.ForfeitedPlayers) then

                            player st.playerTurn +  1u
                         else st.playerTurn +  1u
                player st.playerNumber
    let updatedForfeitedPlayers (st : state) playerId = playerId::st.ForfeitedPlayers
        
    let updatePlayerTurn st =
        let rec aux turn =
            let next = (turn % st.numberOfPlayers) + 1u
            match List.contains next st.ForfeitedPlayers with
            | true -> aux next
            | _ -> next
        aux st.playerTurn
    let changeTurn (st:state) = (st.playerTurn % st.numberOfPlayers) + 1u
    let updateStatePlaySuccess st ms points newPieces =
                let usedTileIds = getUsedTileIdFromMove ms
                let currentHand = removeFromHandSet usedTileIds st.hand  
                let nextHand = addToHandSet newPieces currentHand
                
                let newBoardTiles = getCoordAndTileFromMove ms st.boardTiles
                mkState st.board st.dict st.numberOfPlayers st.playerNumber (changePlayerTurn st) nextHand st.ForfeitedPlayers newBoardTiles
    
    let updateStatePlayed st pid ms points =
        let newBoardTiles = getCoordAndTileFromMove ms st.boardTiles
        mkState st.board st.dict st.numberOfPlayers st.playerNumber (changePlayerTurn st) st.hand st.ForfeitedPlayers newBoardTiles
    
    let  updateStatePlayerPassed st =
        {st with playerTurn = changePlayerTurn st}
    
    let updateStatePlayerForfeit st playerId =
       let updatedForfeitedPlayers = playerId::st.ForfeitedPlayers
       {st with playerTurn = changePlayerTurn st; ForfeitedPlayers = updatedForfeitedPlayers}
       
    let updateStatePiecesChangedSuccess st newPieces =
        let nextHand = addToHandSet newPieces MultiSet.empty
        {st with playerTurn = changePlayerTurn st; hand= nextHand}
         
        
             
    // '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )
    (*HEURISTIC*)
    (*let bestMove (st : state) =
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
                    *)
