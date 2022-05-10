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
        tiles : Map<uint32, tile>

        //number of players (så vi bl.a ved at hvis en forfeiter, og de kun er 2, så har den anden vundet.
        //player turn
        //hvordan holdes der styr på point? - det gør serveren.
    }

    let mkState b d np pn pt h g bT hp vp tl= 
        {board = b; 
        dict = d; 
        numberOfPlayers = np; 
        playerNumber = pn; 
        ForfeitedPlayers =g; 
        playerTurn = pt; hand = h; 
        placedTiles = bT;
        horizontalPrefixes = hp;
        verticalPrefixes = vp;
        tiles  = tl}
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
    let tiles st = st.tiles
    
    // move: ((int * int) * (uint32 * (char * int))) list
    //helper methods

    type directionClear =
        |Top
        |Left
        |Both
        |Neither

    let getDirectionClear (c:coord) (st:state) :directionClear =
        let topClear =  Map.containsKey (fst(c),snd(c)+1) st.placedTiles
        let leftClear = Map.containsKey (fst(c)+1,snd(c)) st.placedTiles
        match topClear, leftClear with
        |true, true -> Both
        |false, false -> Neither
        |true, false -> Top
        |false, true  -> Left

    let isHorizontal (ms:((int * int) * (uint * (char * int))) list) (st:state) =
        if ms.Length = 1 then
            let directionClear = getDirectionClear (coord(fst(List.head ms))) st
            match directionClear with
            |Neither -> failwith "Edge case is both horizontal and vertical prefix"
            |Top -> true
            |Left -> false 
            | _ -> failwith "impossible"
        else
            let firstX = (List.head ms |> fst |> fst)
            let lastX =  (List.last ms |> fst |> fst)
            not(firstX = lastX)

    let rec findStartIndexHorizontal (x,y) prefixes =
        match Map.containsKey (x,y) prefixes with
        | false -> findStartIndexHorizontal (x-1,y) prefixes
        | true -> coord(x,y)

    let rec findStartIndexVertical (x,y) prefixes =
        match Map.containsKey (x,y) prefixes with
        | false -> findStartIndexVertical (x,y-1) prefixes
        | true -> coord(x,y)

    let addOpposite (prefixes) (placedTiles) (ms:((int * int) * (uint * (char * int))) list) isHorizontal = 
        List.fold (fun acc ((x,y),(tile)) -> 
                let coordDecrement =
                    if isHorizontal then (x-1,y)
                    else (x,y-1)
                
                match Map.containsKey coordDecrement placedTiles with
                | false -> Map.add (x,y) [((x,y),(tile))] acc
                | true -> 
                    let startCord = 
                        match isHorizontal with 
                        | true -> findStartIndexHorizontal (x,y) prefixes
                        | false -> findStartIndexVertical (x,y) prefixes
                    let oldPrefix = Map.find (startCord) prefixes
                    let newMap = Map.remove (startCord) prefixes
                    Map.add (startCord) (oldPrefix @ [((x,y),(tile))])newMap
                    ) prefixes ms
                
    let addHorizontalPrefix (x,y) (st:state) (ms:((int * int) * (uint * (char * int))) list)  = 
        match Map.containsKey (x-1,y) st.placedTiles with
        | false -> 
             //Only first move
            //This move is a new prefix. //Only first move
            let newHorizontal = Map.add (x,y) ms st.horizontalPrefixes
            let newVertical = addOpposite st.verticalPrefixes st.placedTiles ms false
            newHorizontal,newVertical
        | true ->
            //This move is an extension of a prefix
            let startCord = findStartIndexHorizontal (x,y) st.horizontalPrefixes
            let oldPrefix = Map.find startCord st.horizontalPrefixes
            let newMap = Map.remove startCord st.horizontalPrefixes
            let newHorizontal = Map.add startCord (oldPrefix @ ms) newMap
            let newVertical = addOpposite st.verticalPrefixes st.placedTiles ms false
            newHorizontal, newVertical
    
    let addVerticalPrefix (x,y) (st:state) (ms:((int * int) * (uint * (char * int))) list)  = 
        match Map.containsKey (x,y-1) st.placedTiles with
        | false -> 
            //This move is a new prefix
            let newVertical = Map.add (x,y) ms st.verticalPrefixes
            let newHorizontal = addOpposite  st.horizontalPrefixes st.placedTiles ms true
            newHorizontal, newVertical
        | true ->
            //This move is an extension of a prefix
            let startCord = findStartIndexVertical (x,y) st.verticalPrefixes
            let oldPrefix = Map.find (startCord) st.verticalPrefixes
            let newMap = Map.remove startCord st.verticalPrefixes
            let newVertical = Map.add startCord (List.append oldPrefix ms) newMap
            let newHorizontal = addOpposite  st.horizontalPrefixes st.placedTiles ms true
            newHorizontal, newVertical

    let addPrefix (startCoord:coord) (st:state) (ms:((int * int) * (uint * (char * int))) list) = 
        match isHorizontal ms st with
        |true -> 
            addHorizontalPrefix (Coord.getX startCoord, Coord.getY startCoord) st ms
        |false ->
            addVerticalPrefix (Coord.getX startCoord, Coord.getY startCoord) st ms
     
    let getWordAndCoordFromMoveHorizontol moveList placedWords =
        List.fold (fun acc (x,k) -> Map.add (coord(x), moveList))
        
    let getCoordAndTileFromMove moveList (placedTiles: Map<coord,(uint*(char*int))>) = 
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
                ScrabbleUtil.DebugPrint.forcePrint $"Got Used Tile ids: %A{usedTileIds}\n"
                
                ScrabbleUtil.DebugPrint.forcePrint $"Removing from hand\n" 
                let currentHand = removeFromHandSet usedTileIds st.hand  
                
                ScrabbleUtil.DebugPrint.forcePrint $"Adding new pieces to hand: %A{newPieces}\n" 
                let nextHand = addToHandSet newPieces currentHand      

                ScrabbleUtil.DebugPrint.forcePrint $"Adding new tile to the board. Old board: %A{st.placedTiles}\n" 
                let newBoardTiles = getCoordAndTileFromMove ms st.placedTiles
                ScrabbleUtil.DebugPrint.forcePrint $"new board: %A{newBoardTiles}\n" 

                ScrabbleUtil.DebugPrint.forcePrint $"Finding startcoord for prefix\n" 
                let prefixStartCoord = ms.Head |> fst

                ScrabbleUtil.DebugPrint.forcePrint $"Adding new prefix\n" 
                let newPrefixes = addPrefix prefixStartCoord st ms

                ScrabbleUtil.DebugPrint.forcePrint $"Saving state\n" 
                {st with 
                    playerTurn = changePlayerTurn st; 
                    hand= nextHand;
                    placedTiles=newBoardTiles;
                    horizontalPrefixes = newPrefixes |> fst; 
                    verticalPrefixes = newPrefixes |> snd}
                    
    
    let updateStatePlayed st pid ms points =
        let newBoardTiles = getCoordAndTileFromMove ms st.placedTiles
        let newPrefixes = addPrefix (fst ms.Head) st ms
        {st with 
            playerNumber=changePlayerTurn st; 
            placedTiles = newBoardTiles;
            horizontalPrefixes = newPrefixes |> fst; 
            verticalPrefixes = newPrefixes |> snd}
    
    let  updateStatePlayerPassed st =
        {st with playerTurn = changePlayerTurn st}
    
    let updateStatePlayerForfeit st playerId =
       let updatedForfeitedPlayers = playerId::st.ForfeitedPlayers
       {st with playerTurn = changePlayerTurn st; ForfeitedPlayers = updatedForfeitedPlayers}
       
    let updateStatePiecesChangedSuccess st newPieces =
        let nextHand = addToHandSet newPieces MultiSet.empty
        {st with playerTurn = changePlayerTurn st; hand= nextHand}
         
