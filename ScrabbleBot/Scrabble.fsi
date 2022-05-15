namespace madsSW2
open ScrabbleUtil
open System.IO
module Scrabble =

    val startGame :
        boardProg ->                 (* Scrabble board *)
        (bool -> Dictionary.Dict) -> (* Dictionary (call with true if using a Gaddag, and false if using a Trie) *)
        uint32 ->                    (* Number of players *)
        uint32 ->                    (* Your player number *)
        uint32 ->                    (* starting player number *)
        (uint32 * uint32) list ->    (* starting hand (tile id, number of tiles) *)
        Map<uint32, tile> ->         (* Tile lookup table *)
        uint32 option ->             (* Timeout in miliseconds *)
        Stream ->                    (* Communication channel to the server *)
        (unit -> 'a)             (* Delay to allow everyone to start at the same time after setup *)
