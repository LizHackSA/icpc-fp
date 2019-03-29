module ICPC
open System

let findWord theList wordToLook = 
    let rec findWordAndChange list accList =
        match list with
        | [] -> (String.concat " " (List.rev accList))
        | head::SecondPos::theRest -> 
            let indexOfPeriod = head.ToString().IndexOf('.')
            match (wordToLook=head) with
            | true -> findWordAndChange theRest ((head+",")::accList)
            | false -> findWordAndChange theRest (accList)
    findWordAndChange theList []

let findIt theList = 
    let rec getTheWord thelist accWord =
        match thelist with 
        | [] -> accWord
        | a::c -> 
            let indexOfComma = a.ToString().IndexOf(',')
            match indexOfComma with
            | -1 -> getTheWord c "Empty"
            | _ -> (a.ToString().Trim(','))
    getTheWord theList "Empty"

let AddComma inputString word = 
    let rec PutItIn (theString:string) accPos theAccString = 
        let index = theString.IndexOf(word:string)
        match accPos < theString.ToString().Length with
        | false -> theAccString
        | true -> PutItIn (theString.ToString().Substring(index)) (accPos+1) (theString.ToString().Insert((index+1), ","))
    PutItIn inputString 0 ""

let commaSprinkler input =
    let indexOfComma = input.ToString().IndexOf(',')
    match indexOfComma with
    | -1 | 0 -> None
    | _ -> 
        let name = findWord (input.ToString().Split(' ')|> Array.toList) (findIt (input.ToString().Split(' ')|> Array.toList)) 
        match name=String.Empty with
        | true -> None
        | false -> 
            let s = AddComma input (findIt (input.ToString().Split(' ')|> Array.toList))
            Some s
            //Some name
        
    

let rivers input =
    failwith "Not implemented"

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
