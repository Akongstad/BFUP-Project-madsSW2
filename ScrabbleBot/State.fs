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
        placedTiles    : Map<coord,(uint*(char*int))>
        horizontalPrefixes : Map<coord,((int * int) * (uint * (char * int))) list>
        verticalPrefixes : Map<coord,((int * int) * (uint * (char * int))) list>

        //number of players (så vi bl.a ved at hvis en forfeiter, og de kun er 2, så har den anden vundet.
        //player turn
        //hvordan holdes der styr på point? - det gør serveren.
    }

    let mkState b d np pn pt h g bT hp vp= 
        {board = b; 
        dict = d; 
        numberOfPlayers = np; 
        playerNumber = pn; 
        ForfeitedPlayers =g; 
        playerTurn = pt; hand = h; 
        placedTiles = bT;
        horizontalPrefixes = hp;
        verticalPrefixes = vp}
    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand
    let playerTurn st = st.playerTurn
    let numberOfPlayers st = st.numberOfPlayers
    let ForfeitedPlayers st = st.ForfeitedPlayers
    let placedTiles st = st.placedTiles
    let horizontalPrefixes st = st.horizontalPrefixes
    let verticalPrefixes st = st.horizontalPrefixes
    
    // move: ((int * int) * (uint32 * (char * int))) list
    //helper methods

    let isHorizontal (ms:((int * int) * (uint * (char * int))) list) =
        (List.head ms |> fst |> fst) = (List.last ms |> fst |> fst)

    let rec findStartIndexHorizontal (x,y) (st:state) =
        match st.horizontalPrefixes.ContainsKey (x,y) with
        | false -> findStartIndexHorizontal (x-1,y) st
        | true -> coord(x,y)

    let rec findStartIndexVertical (x,y) (st:state) =
        match st.verticalPrefixes.ContainsKey (x,y) with
        | false -> findStartIndexVertical (x,y-1) st
        | true -> coord(x,y)

    let addOpposite (x,y) (st:state) (ms:((int * int) * (uint * (char * int))) list) = 
        List.fold (fun acc ((x,y),(_)) -> 
                match Map.containsKey (x,y-1) st.placedTiles with
                | false -> Map.add (x,y) ms st.verticalPrefixes
                | true -> 
                    let startCord = findStartIndexVertical (x,y) st
                    let oldPrefix = Map.find (startCord) st.verticalPrefixes
                    let newMap = st.verticalPrefixes.Remove (startCord)
                    Map.add (startCord) (List.append oldPrefix ms)newMap) Map.empty ms
                
    let addHorizontal (x,y) (st:state) (ms:((int * int) * (uint * (char * int))) list) = 
        match Map.containsKey (x-1,y) st.placedTiles with
        | false -> 
            //This move is a new prefix
            let newHorizontal = Map.add (x,y) ms st.horizontalPrefixes
            let newVertical = addOpposite (x,y) st ms
            newHorizontal,newVertical
        | true ->
            //This move is an extension of a prefix
            let startCord = findStartIndexHorizontal (x,y) st
            let oldPrefix = Map.find (startCord) st.horizontalPrefixes
            let newMap = st.horizontalPrefixes.Remove startCord
            let newHorizontal = Map.add startCord (List.append oldPrefix ms) newMap
            newHorizontal, addOpposite (x,y) st ms


    let addMoveAsPrefix (startCoord:coord) (st:state) (ms:((int * int) * (uint * (char * int))) list) = 
        match isHorizontal ms with
        |true -> 
            let currentWord = st.horizontalPrefixes[startCoord]
            st.horizontalPrefixes[startCoord] = currentWord @ ms
            st.horizontalPrefixes
        |false ->
            let currentWord = st.verticalPrefixes[startCoord]
            st.verticalPrefixes[startCoord] = currentWord @ ms
            st.verticalPrefixes
     
    let getWordAndCoordFromMoveHorizontol moveList placedWords =
        List.fold (fun acc (x,k) -> Map.add (coord(x), moveList))
        
    let getCoordAndTileFromMove moveList placedTiles = 
        List.fold (fun acc (x,k) -> Map.add (coord(x)) (k) acc) placedTiles moveList //get list of coord and (char * char point val)
        
    let getUsedTileIdFromMove moveList = 
        List.fold (fun acc (x,k) -> fst(k)::acc) List.Empty moveList //get the tile ids of the played move
    
    let removeFromHandSet playedPieces hand = 
        List.fold (fun acc x -> MultiSet.remove x 1u acc) hand playedPieces //removes the played tiles from the hand
    
    let addToHandSet newTiles hand = 
        List.fold (fun acc (x, k) -> MultiSet.add x k acc) hand newTiles //adds new tiles to the hand
    
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
                let newBoardTiles = getCoordAndTileFromMove ms st.placedTiles
                {st with playerTurn = changePlayerTurn st; hand= nextHand; placedTiles=newBoardTiles}
    
    let updateStatePlayed st pid ms points =
        let newBoardTiles = getCoordAndTileFromMove ms st.placedTiles
        {st with playerNumber=changePlayerTurn st; placedTiles=newBoardTiles }
    
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
        let placedTiles = st.placedTiles
        
        
        let coords = List.ofSeq placedTiles.Keys
        
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
                    aux coords.Head placedTiles[coords.Head]
                |Both -> dunno
                |Top ->
                    let bka = step 'c' dict
                    *)
