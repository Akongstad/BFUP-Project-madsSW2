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

    type extendDirection =
        |Vertical
        |Horizontal
        |Both
        |Neither

    let getDirection (c:coord) placedTiles =
        
        let extendsHorizontal = Map.containsKey (fst(c)-1,snd(c)) placedTiles
        let prependsHorizontal = Map.containsKey (fst(c)+1,snd(c)) placedTiles
        
        let extendsVertical =  Map.containsKey (fst(c),snd(c)-1) placedTiles
        let prependsVertical = Map.containsKey (fst(c),snd(c)+1) placedTiles
        
        match extendsHorizontal || prependsHorizontal, extendsVertical || prependsVertical with
        |true, true -> Neither
        |false, false -> Both
        |true, false -> Horizontal
        |false, true  -> Vertical

    let sortMs (ms:((int * int) * (uint * (char * int))) list) isHorizontal  =
        match isHorizontal with
        |true -> List.sortBy (fun ((x,_),(_,(_,_))) -> x ) ms
        |false -> List.sortBy (fun ((_,y),(_,(_,_))) -> y ) ms
    let isHorizontal (ms:((int * int) * (uint * (char * int))) list) placedTiles =
        if ms.Length = 1 then
            let direction = getDirection (coord(fst(List.head ms))) placedTiles
            match direction with
            |Neither ->
                    DebugPrint.forcePrint $"Edge case is both horizontal and vertical prefix. Extends both so direction does not matter"
                    true
            |Vertical -> false
            |Horizontal -> true 
            | _ -> failwith "impossible"
        else
            let firstX = (List.head ms |> fst |> fst)
            let lastX =  (List.last ms |> fst |> fst)
            not(firstX = lastX)

    let rec findStartIndexHorizontal (x,y) prefixes =
        match Map.containsKey (x,y) prefixes with
        | false -> findStartIndexHorizontal (x-1,y) prefixes
        | true -> 
            DebugPrint.forcePrint $"findstartIndexHorizontal: %A{coord(x,y)}"
            coord(x,y)

    let rec findStartIndexVertical (x,y) prefixes =
        match Map.containsKey (x,y) prefixes with
        | false -> findStartIndexVertical (x,y-1) prefixes
        | true -> 
            DebugPrint.forcePrint $"findstartIndexVerical: %A{coord(x,y)}"
            coord(x,y)



    let getCoordAndTileFromMove moveList (placedTiles: Map<coord,(uint*(char*int))>) = 
        List.fold (fun acc (x,k) -> Map.add (coord(x)) (k) acc) placedTiles moveList //get list of coord and (char * char point val)
        

    let getNewSearchCoord (x,y) isHorizontal isSuffix = 
            match isHorizontal with
            | false -> 
                match isSuffix with 
                |false -> (x,y-1)
                |true -> (x,y+1) 
            | true -> 
                match isSuffix with 
                |false -> (x-1,y)
                |true -> (x+1,y) 
    
    let getFixFromMove placedTiles prefixes (ms:((int * int) * (uint * (char * int))) list) isHorizontal isSuffix newPrefixes= 
        let (x,y) = Coord.getX(ms.Head |> fst), Coord.getY((ms.Head)|> fst)
        
        let rec aux placedTiles prefixes coords isHorizontal =
            let newCoord = getNewSearchCoord coords isHorizontal isSuffix
            match Map.containsKey (coord(newCoord)) placedTiles with
            | false -> newPrefixes
            | true when Map.containsKey (coord(newCoord)) prefixes ->
                let prefix = Map.find (coord(newCoord)) prefixes
                Map.add (coord(newCoord)) prefix newPrefixes
            | true -> aux placedTiles prefixes newCoord isHorizontal
        aux placedTiles prefixes (x,y) isHorizontal 

    let rec getMidfixesFromMove prefixes (ms:((int * int) * (uint * (char * int))) list) isHorizontal acc = 
        let headCoord = coord(fst ms.Head)
        let tailCoord = coord(fst ms.Tail.Head)
        DebugPrint.forcePrint $"headCoord %A{headCoord}\n"
        DebugPrint.forcePrint $"tailCoord %A{tailCoord}\n"
        let diff = 
            match isHorizontal with
            | false -> 
                (Coord.getY headCoord - Coord.getY tailCoord)
            | true -> 
                (Coord.getX headCoord - Coord.getX tailCoord)   
        //forcePrint.forcePrint $"diff %d{diff}\n"
        match List.length ms with
        | 2 -> 
            match  ms with
            | [] -> acc //will never happen
            | _ when not(System.Math.Abs diff = 1) -> 
                let oldPrefixCoords = 
                    match isHorizontal with
                    | false -> 
                        coord(Coord.getX headCoord, Coord.getY headCoord + 1)
                    | true -> 
                        coord(Coord.getX headCoord + 1, Coord.getY headCoord)
                DebugPrint.forcePrint $"diff %d{diff} coord: %A{oldPrefixCoords}>\n"
                
                let prefix = Map.find oldPrefixCoords prefixes
                DebugPrint.forcePrint $"Prefix: %A{prefix}\n"
                
                Map.add oldPrefixCoords prefix acc
            | _ -> acc
        | _ -> 
            match  ms with
            | [] -> acc
            | _::t when not(System.Math.Abs diff = 1) -> 
                let oldPrefixCoords = 
                    match isHorizontal with
                    | false -> 
                        coord(Coord.getX headCoord, Coord.getY headCoord + 1)
                    | true -> 
                        coord(Coord.getX headCoord + 1, Coord.getY headCoord)

                DebugPrint.forcePrint $"diff %d{diff} coord: %A{oldPrefixCoords}>\n"
                
                let prefix = Map.find oldPrefixCoords prefixes
                DebugPrint.forcePrint $"Prefix: %A{prefix}\n"
                
                let newAcc = Map.add oldPrefixCoords prefix acc
                getMidfixesFromMove prefixes t isHorizontal newAcc
            | _::t -> getMidfixesFromMove prefixes t isHorizontal acc
        
    let getFixesFromMove (prefixes:Map<coord,((int * int) * (uint * (char * int))) list>) (placedTiles: Map<coord,(uint*(char*int))>) (ms:((int * int) * (uint * (char * int))) list) isHorizontal  = 
        //ScrabbleUtil.DebugPrint.forcePrint $"Get midfixes\n" 
        //ScrabbleUtil.DebugPrint.forcePrint $"ms in getFixes from move: %A{ms}\n" 
        let midFixes =
            match List.length ms with
            | 1 -> Map.empty
            | _ -> getMidfixesFromMove prefixes ms isHorizontal (Map.empty)

        //ScrabbleUtil.DebugPrint.forcePrint $"Get prefixes. midfixes: %A{midFixes}\n" 
        let midAndPrefixes = getFixFromMove (getCoordAndTileFromMove ms placedTiles) prefixes ms isHorizontal false midFixes
        DebugPrint.forcePrint $"Get suffixes. fixes: %A{midAndPrefixes}\n" 
        let allFixes = getFixFromMove (getCoordAndTileFromMove ms placedTiles) prefixes ms isHorizontal true midAndPrefixes
        DebugPrint.forcePrint $"All fixes %A{allFixes}\n" 
        allFixes
    
    let addOpposite (prefixes) (placedTiles) (ms:((int * int) * (uint * (char * int))) list) isHorizontal = 
        List.fold (fun acc msItem -> 
            let newFixes = getFixesFromMove acc placedTiles ([msItem]) isHorizontal
            let oppositePrefixesCleaned = 
                Map.fold (fun acc coords _ -> 
                Map.remove coords acc ) acc newFixes
            let word = 
                Map.fold (fun acc _ elem -> 
                elem @ acc
                ) [msItem] newFixes
            let wordSorted = sortMs word isHorizontal
            let startCoord = wordSorted.Head |> fst
            Map.add startCoord wordSorted oppositePrefixesCleaned
            ) prefixes ms
                
    

        (* List.fold (fun acc ((x,y),(tile)) -> 
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
                    ) prefixes ms *)

    let addHorizontalPrefix (x,y) (st:state) (ms:((int * int) * (uint * (char * int))) list)  = 
        match Map.isEmpty st.placedTiles with
        | true -> 
             //Only first move
            //This move is a new prefix. //Only first move
            let newHorizontal = Map.add (x,y) ms st.horizontalPrefixes
            let newVertical = addOpposite st.verticalPrefixes st.placedTiles ms false
            newHorizontal,newVertical
        | false ->
            //ScrabbleUtil.DebugPrint.forcePrint $"Get fixes called Horizontally: ms: ‰A{ms} \n" 
            //ScrabbleUtil.DebugPrint.forcePrint $"Get fixes\n" 
            let newFixes = getFixesFromMove st.horizontalPrefixes st.placedTiles ms true
            let horizontalPrefixesCleaned = 
                Map.fold (fun acc coords elem -> 
                Map.remove coords acc ) st.horizontalPrefixes newFixes
            //This move is an extension of a prefix
            let word = 
                Map.fold (fun acc _ elem -> 
                elem @ acc
                ) ms newFixes
            let wordSorted = sortMs word true
            let startCoord = wordSorted.Head |> fst

            let newHorizontal = Map.add startCoord wordSorted horizontalPrefixesCleaned
            let newVertical = addOpposite st.verticalPrefixes st.placedTiles ms false
            newHorizontal, newVertical
    
    let addVerticalPrefix (x,y) (st:state) (ms:((int * int) * (uint * (char * int))) list)  = 
        match Map.isEmpty st.placedTiles with
        | true ->
            //ScrabbleUtil.forcePrint.forcePrint $"pls no \n" 
            //This move is a new prefix        
            let newVertical = Map.add (x,y) ms st.verticalPrefixes
            let newHorizontal = addOpposite  st.horizontalPrefixes st.placedTiles ms true
            newHorizontal, newVertical
        | false ->
            //ScrabbleUtil.DebugPrint.forcePrint $"Get fixes called vertically: ms: ‰A{ms} \n" 
            //ScrabbleUtil.DebugPrint.forcePrint $"Get fixes\n" 
            let newFixes = getFixesFromMove st.verticalPrefixes st.placedTiles ms false
            
            let VerticalPrefixesCleaned = 
                Map.fold (fun acc coords elem -> 
                Map.remove coords acc ) st.verticalPrefixes newFixes
            //This move is an extension of a prefix
            let word = 
                Map.fold (fun acc _ elem -> 
                elem @ acc
                ) ms newFixes
            let wordSorted = sortMs word false
            let startCoord = wordSorted.Head |> fst

            let newVertical = Map.add startCoord wordSorted VerticalPrefixesCleaned
            let newHorizontal = addOpposite st.horizontalPrefixes st.placedTiles ms true 
            newHorizontal, newVertical

    let addPrefix (startCoord:coord) (st:state) (ms:((int * int) * (uint * (char * int))) list) isHorizontal = 
        match isHorizontal with
        |true -> 
            addHorizontalPrefix (Coord.getX startCoord, Coord.getY startCoord) st ms
        |false ->
            addVerticalPrefix (Coord.getX startCoord, Coord.getY startCoord) st ms
     
    let getWordAndCoordFromMoveHorizontol moveList placedWords =
        List.fold (fun acc (x,k) -> Map.add (coord(x), moveList))
        
    let getUsedTileIdFromMove moveList = 
        List.fold (fun acc (x,k) -> fst(k)::acc) List.Empty moveList //get the tile ids of the played move
    
    let removeFromHandSet playedPieces hand = 
        List.fold (fun acc x -> MultiSet.remove x 1u acc) hand playedPieces //removes the played tiles from the hand
    
    let addToHandSet newTiles hand = 
        List.fold (fun acc (x, k) -> MultiSet.add x k acc) hand newTiles //adds new tiles to the hand
    
    let updatedForfeitedPlayers (st : state) playerId = playerId::st.ForfeitedPlayers
        
    let updatePlayerTurn st =
        let rec aux turn =
            let next = (turn % st.numberOfPlayers) + 1u
            match List.contains next st.ForfeitedPlayers with
            | true -> aux next
            | _ -> next
        aux st.playerTurn
    let updateStatePlaySuccess st ms points newPieces =

                let isHorizontal = isHorizontal ms st.placedTiles
                let msSorted = sortMs ms isHorizontal

                let usedTileIds = getUsedTileIdFromMove msSorted
                //ScrabbleUtil.forcePrint.forcePrint $"Got Used Tile ids: %A{usedTileIds}\n"
                
                //ScrabbleUtil.forcePrint.forcePrint $"Removing from hand\n" 
                let currentHand = removeFromHandSet usedTileIds st.hand  
                
                //ScrabbleUtil.forcePrint.forcePrint $"Adding new pieces to hand: %A{newPieces}\n" 
                let nextHand = addToHandSet newPieces currentHand      

                //ScrabbleUtil.forcePrint.forcePrint $"Adding new tile to the board. Old board: %A{st.placedTiles}\n" 
                let newBoardTiles = getCoordAndTileFromMove msSorted st.placedTiles
                //ScrabbleUtil.forcePrint.forcePrint $"new board: %A{newBoardTiles}\n" 

                //ScrabbleUtil.forcePrint.forcePrint $"Finding startcoord for prefix\n" 
                let prefixStartCoord = msSorted.Head |> fst
                //ScrabbleUtil.DebugPrint.forcePrint $"ms Sorted: %A{msSorted}\n" 
                //ScrabbleUtil.forcePrint.forcePrint $"Adding new prefix: StartCoord = %A{prefixStartCoord}\n" 
                let newPrefixes = addPrefix prefixStartCoord st msSorted isHorizontal
                //ScrabbleUtil.forcePrint.forcePrint $"new board: %A{newBoardTiles}\n" 

                //ScrabbleUtil.DebugPrint.forcePrint $"Saving state\n" 
                {st with 
                    playerTurn = updatePlayerTurn st; 
                    hand= nextHand;
                    placedTiles=newBoardTiles;
                    horizontalPrefixes = newPrefixes |> fst; 
                    verticalPrefixes = newPrefixes |> snd}
            
                        
    
    let updateStatePlayed st pid ms points =
        ////ScrabbleUtil.DebugPrint.forcePrint $"Getting direction\n" 
        let isHorizontal = isHorizontal ms st.placedTiles
        ////ScrabbleUtil.DebugPrint.forcePrint $"Direction: %b{isHorizontal}\n" 
        //ScrabbleUtil.DebugPrint.forcePrint $"Sorting ms\n" 
        let msSorted = sortMs ms isHorizontal

        //ScrabbleUtil.DebugPrint.forcePrint $"Finding new board tiles\n" 
        let newBoardTiles = getCoordAndTileFromMove msSorted st.placedTiles
        //ScrabbleUtil.DebugPrint.forcePrint $"ms Sorted: %A{msSorted}\n" 
        let prefixStartCoord = msSorted.Head |> fst
        let newPrefixes = addPrefix prefixStartCoord st msSorted isHorizontal
        
        {st with 
            playerTurn=updatePlayerTurn st; 
            placedTiles = newBoardTiles;
            horizontalPrefixes = newPrefixes |> fst; 
            verticalPrefixes = newPrefixes |> snd}
    
    let  updateStatePlayerPassed st =
        {st with playerTurn = updatePlayerTurn st}
    
    let updateStatePlayerForfeit st playerId =
       let updatedForfeitedPlayers = playerId::st.ForfeitedPlayers
       {st with playerTurn = updatePlayerTurn st; ForfeitedPlayers = updatedForfeitedPlayers}
       
    let updateStatePiecesChangedSuccess st newPieces =
        let nextHand = addToHandSet newPieces MultiSet.empty
        {st with playerTurn = updatePlayerTurn st; hand= nextHand}
         
