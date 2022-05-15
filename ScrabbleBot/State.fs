namespace madsSW2
open Parser
open ScrabbleUtil
module internal State =  
    type state = {
        board         : board
        dict          : Dictionary.Dict
        numberOfPlayers : uint32
        ForfeitedPlayers : List<uint32>
        playerNumber  : uint32
        playerTurn    : uint32
        hand          : MultiSet.MultiSet<uint32>
        placedTiles    : Map<coord,uint*(char*int)>
        horizontalPrefixes : Map<coord,((int * int) * (uint * (char * int))) list>
        verticalPrefixes : Map<coord,((int * int) * (uint * (char * int))) list>
        tiles : Map<uint32, tile>
        timeout: uint32 option
        allowChange: bool
    }

    let mkState b d np pn pt h g bT hp vp tl tm ac= 
        {board = b; 
        dict = d; 
        numberOfPlayers = np; 
        playerNumber = pn; 
        ForfeitedPlayers =g; 
        playerTurn = pt; hand = h; 
        placedTiles = bT;
        horizontalPrefixes = hp;
        verticalPrefixes = vp;
        tiles  = tl;
        timeout = tm;
        allowChange = ac}
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
    let timeout st = st.timeout
    let allowChange st = st.allowChange
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
                    //"Extends both so direction does not matter"
                    true
            |Vertical -> false
            |Horizontal -> true 
            | _ -> failwith "impossible"
        else
            let firstX = (List.head ms |> fst |> fst)
            let lastX =  (List.last ms |> fst |> fst)
            not(firstX = lastX)

    let getCoordAndTileFromMove moveList (placedTiles: Map<coord,uint*(char*int)>) = 
        List.fold (fun acc (x,k) -> Map.add (coord(x)) k acc) placedTiles moveList //get list of coord and (char * char point val)
        
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
        let x,y = Coord.getX(ms.Head |> fst), Coord.getY(ms.Head |> fst)
        
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
        let diff = 
            match isHorizontal with
            | false -> 
                (Coord.getY headCoord - Coord.getY tailCoord)
            | true -> 
                (Coord.getX headCoord - Coord.getX tailCoord)   

        match  ms with
        | [] -> acc //will never happen
        | _ when List.length ms = 2 && not(System.Math.Abs diff = 1) -> 
            let oldPrefixCoords = 
                match isHorizontal with
                | false -> 
                    coord(Coord.getX headCoord, Coord.getY headCoord + 1)
                | true -> 
                    coord(Coord.getX headCoord + 1, Coord.getY headCoord)            
            let prefix = Map.find oldPrefixCoords prefixes
            
            Map.add oldPrefixCoords prefix acc
        | _ when List.length ms = 2 -> acc
        | _::t when not(System.Math.Abs diff = 1) -> 
            let oldPrefixCoords = 
                match isHorizontal with
                | false -> 
                    coord(Coord.getX headCoord, Coord.getY headCoord + 1)
                | true -> 
                    coord(Coord.getX headCoord + 1, Coord.getY headCoord)
            
            let prefix = Map.find oldPrefixCoords prefixes            
            let newAcc = Map.add oldPrefixCoords prefix acc
            getMidfixesFromMove prefixes t isHorizontal newAcc
        | _::t -> getMidfixesFromMove prefixes t isHorizontal acc
        
    let getFixesFromMove (prefixes:Map<coord,((int * int) * (uint * (char * int))) list>) (placedTiles: Map<coord,uint*(char*int)>) (ms:((int * int) * (uint * (char * int))) list) isHorizontal  = 
        let midFixes =
            match List.length ms with
            | 1 -> Map.empty
            | _ -> getMidfixesFromMove prefixes ms isHorizontal Map.empty

        let midAndPrefixes = getFixFromMove (getCoordAndTileFromMove ms placedTiles) prefixes ms isHorizontal false midFixes
        let allFixes = getFixFromMove (getCoordAndTileFromMove ms placedTiles) prefixes ms isHorizontal true midAndPrefixes
        allFixes
    
    let addOpposite prefixes placedTiles (ms:((int * int) * (uint * (char * int))) list) isHorizontal = 
        List.fold (fun acc msItem -> 
            let newFixes = getFixesFromMove acc placedTiles [msItem] isHorizontal
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

    let addHorizontalPrefix (x,y) (st:state) (ms:((int * int) * (uint * (char * int))) list)  = 
        match Map.isEmpty st.placedTiles with
        | true -> 
             //Only first move
            //This move is a new prefix. //Only first move
            let newHorizontal = Map.add (x,y) ms st.horizontalPrefixes
            let newVertical = addOpposite st.verticalPrefixes st.placedTiles ms false
            newHorizontal,newVertical
        | false ->
            let newFixes = getFixesFromMove st.horizontalPrefixes st.placedTiles ms true
            let horizontalPrefixesCleaned = 
                Map.fold (fun acc coords _ -> 
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
            //This move is a new prefix        
            let newVertical = Map.add (x,y) ms st.verticalPrefixes
            let newHorizontal = addOpposite  st.horizontalPrefixes st.placedTiles ms true
            newHorizontal, newVertical
        | false ->
            let newFixes = getFixesFromMove st.verticalPrefixes st.placedTiles ms false
            
            let VerticalPrefixesCleaned = 
                Map.fold (fun acc coords _ -> 
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
        
    let getUsedTileIdFromMove moveList = 
        List.fold (fun acc (_,k) -> fst(k)::acc) List.Empty moveList //get the tile ids of the played move
    
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
    let updateStatePlaySuccess st ms _ newPieces =
                let isHorizontal = isHorizontal ms st.placedTiles
                let msSorted = sortMs ms isHorizontal
                let usedTileIds = getUsedTileIdFromMove msSorted
                let currentHand = removeFromHandSet usedTileIds st.hand  
                let nextHand = addToHandSet newPieces currentHand      
                let newBoardTiles = getCoordAndTileFromMove msSorted st.placedTiles
                let prefixStartCoord = msSorted.Head |> fst
                let newPrefixes = addPrefix prefixStartCoord st msSorted isHorizontal

                {st with 
                    playerTurn = updatePlayerTurn st; 
                    hand= nextHand;
                    placedTiles=newBoardTiles;
                    horizontalPrefixes = newPrefixes |> fst; 
                    verticalPrefixes = newPrefixes |> snd}
            
                        
    
    let updateStatePlayed st _ ms _ =
        let isHorizontal = isHorizontal ms st.placedTiles
        let msSorted = sortMs ms isHorizontal
        let newBoardTiles = getCoordAndTileFromMove msSorted st.placedTiles
        let prefixStartCoord = msSorted.Head |> fst
        let newPrefixes = addPrefix prefixStartCoord st msSorted isHorizontal
        
        {st with 
            playerTurn=updatePlayerTurn st; 
            placedTiles = newBoardTiles;
            horizontalPrefixes = newPrefixes |> fst; 
            verticalPrefixes = newPrefixes |> snd}
    
    let  updateStatePlayerPassed st =
        {st with playerTurn = updatePlayerTurn st}
    
    let  updateStateNotEnoughPieces st =
        {st with playerTurn = updatePlayerTurn st; allowChange = false}
    
    let updateStatePlayerForfeit st playerId =
       let updatedForfeitedPlayers = playerId::st.ForfeitedPlayers
       {st with playerTurn = updatePlayerTurn st; ForfeitedPlayers = updatedForfeitedPlayers}
       
    let updateStatePiecesChangedSuccess st newPieces =
        let nextHand = addToHandSet newPieces MultiSet.empty
        {st with playerTurn = updatePlayerTurn st; hand= nextHand}
         
