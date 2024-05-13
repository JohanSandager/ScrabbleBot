module Program

open Walker // Replace "YourModule" with the actual name of your module
open PieceFormatter

[<EntryPoint>]
let main argv =
    let readLines filePath = System.IO.File.ReadLines(filePath)
    let words = readLines "../ScrabbleTemplate/Dictionaries/English.txt"

    let dictAPI =
        // Uncomment if you have implemented a dictionary. last element None if you have not implemented a GADDAG
        Some(Dict.empty, Dict.insert, Dict.step, None)
    // None
    let ourDict = ScrabbleUtil.Dictionary.mkDict words dictAPI false
    // Your test code here
    let alphabetMap =
        [ 'A' .. 'Z' ] |> List.mapi (fun i c -> uint32 (i + 1), (c, 2)) |> Map.ofList

    let hand = [ 21u; 11u; 3u; 18u; 5u ]
    let prev = [ 6u; 21u; 3u; 11u ]
    let res = getFirstWord hand ourDict alphabetMap prev
    printf "%A \n" (res.ToString())
    printf "%A%A \n" (idLstToString prev alphabetMap) (idLstToString res alphabetMap)
    0 // Return an integer exit code
