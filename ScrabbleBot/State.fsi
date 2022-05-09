namespace madsSW2

open Eval
open Parser
open ScrabbleUtil.Dictionary

module internal State =
    type state = {
            board         : board
            dict          : Dict
            numberOfPlayers : uint32
            ForfeitedPlayers : List<uint32>
            playerNumber  : uint32
            playerTurn    : uint32
            hand          : MultiSet.MultiSet<uint32>
            placedTiles    : Map<coord,(uint*(char*int))>
            horizontalPrefixes : Map<coord,((int * int) * (uint * (char * int))) list>
            verticalPrefixes : Map<coord,((int * int) * (uint * (char * int))) list>
            tiles : Map<uint32, ScrabbleUtil.tile>
            //number of players (så vi bl.a ved at hvis en forfeiter, og de kun er 2, så har den anden vundet.
            //player turn
            //hvordan holdes der styr på point? - det gør serveren.
        }
    
    val mkState : board -> Dict -> uint32 -> uint32 -> uint32 -> MultiSet.MultiSet<uint32> -> List<uint32> -> Map<coord,(uint*(char*int))> -> Map<coord,((int * int) * (uint * (char * int))) list> -> Map<coord,((int * int) * (uint * (char * int))) list> -> Map<uint32, ScrabbleUtil.tile> -> state
    val board           : state -> board
    val dict            : state -> Dict
    val numberOfPlayers : state -> uint32
    val ForfeitedPlayers: state -> List<uint32>
    val playerNumber    : state -> uint32
    val playerTurn      : state -> uint32
    val hand            : state -> MultiSet.MultiSet<uint32>
    val placedTiles      : state -> Map<coord,(uint*(char*int))>
    val horizontalPrefixes : state -> Map<coord,((int * int) * (uint * (char * int))) list>
    val verticalPrefixes : state -> Map<coord,((int * int) * (uint * (char * int))) list> 
    val tiles : state -> Map<uint32, ScrabbleUtil.tile>
    
    val updateStatePlaySuccess : state -> ((int * int) * (uint32 * (char * int))) list -> 'a -> (uint32 * uint32) list -> state
    val updateStatePlayed : state -> 'a -> ((int * int) * (uint * (char * int))) list -> 'c -> state
    val updateStatePlayerPassed : state -> state
    val updateStatePlayerForfeit : state -> uint32 -> state
    val updateStatePiecesChangedSuccess : state -> (uint32 * uint32) list -> state