﻿// Learn more about F# at http://fsharp.org
open System
let time f =
    let start = DateTime.Now
    let res = f ()
    let finish = DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function 
        | 0 -> []
        | x -> (sprintf "%s%d" name x, dict, bot)::aux (x - 1)
   
    aux >> List.rev

[<EntryPoint>]
let main argv =
    ScrabbleUtil.DebugPrint.toggleDebugPrint false // Change to false to supress debug output

    Console.BackgroundColor <- ConsoleColor.White
    Console.ForegroundColor <- ConsoleColor.Black
    Console.Clear()


    //let board        = ScrabbleUtil.StandardBoard.standardBoard ()
    let board      = ScrabbleUtil.InfiniteBoard.infiniteBoard ()
    //let board      = ScrabbleUtil.RandomBoard.randomBoard ()
    //let board      = ScrabbleUtil.RandomBoard.randomBoardSeed (Some 42)
    //let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoard ()
    //let board      = ScrabbleUtil.InfiniteRandomBoard.infiniteRandomBoardSeed (Some 42)
    //let board      = ScrabbleUtil.HoleBoard.holeBoard ()
    //let board      = ScrabbleUtil.InfiniteHoleBoard.infiniteHoleBoard ()
    
    let words     = readLines (AppContext.BaseDirectory + "../../../Dictionaries/English.txt")
    

    let handSize   = 7u
    let timeout    = Some(2000u)
    let tiles      = ScrabbleUtil.English.tiles 1u
    let seed       = None
    let port       = 13001

    let dictAPI = 
        Some (Dictionary.empty, Dictionary.insert, Dictionary.step, None) 

    let dictionary, time =
        time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI)
     
     //Uncomment to play against self
    //let players  =  spawnMultiples "mads" dictionary madsSW2.Scrabble.startGame 4
     
    //Uncomment to play against Oxyphenbutazone
    let madsdrengen  =  spawnMultiples "mads" dictionary madsSW2.Scrabble.startGame 1
    let oxydrengen = spawnMultiples "Oxy" dictionary  Oxyphenbutazone.Scrabble.startGame 1
    let players = madsdrengen @ oxydrengen
    
    //Uncommment to watch OxyphenButazone
    //let players = spawnMultiples "OxyphenButazone" dictionary Oxyphenbutazone.Scrabble.startGame 4

    //Test dictionary implementation. isEmpty == true means passed
    let test = ScrabbleUtil.Dictionary.test words 10 (dictionary false)
    printf $"Testing dictionary... result: {test.IsEmpty} \n"

    do ScrabbleServer.Comm.startGame 
          board dictionary handSize timeout tiles seed port players
    
    ScrabbleUtil.DebugPrint.forcePrint "Server has terminated. Press Enter to exit program.\n"
    Console.ReadLine () |> ignore

    0