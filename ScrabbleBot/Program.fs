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

    let hand = [ 8u; 9u; 5u; 12u; 12u; 15u ]
    let result = tryMakeNewWordFromHand hand [ 8u ] 0 ourDict alphabetMap
    printf "%A" (result.ToString())
    let result = findWordRecursive hand [ 8u ] ourDict alphabetMap
    let word = idLstToString result alphabetMap
    printf "%A" word
    0 // Return an integer exit code
