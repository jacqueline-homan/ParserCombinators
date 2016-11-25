open System
open System.IO

//Parsing a hard-coded char
let a_parser str =
    if String.IsNullOrEmpty(str) then
        (false, "")
    elif str.[0] = 'A' then
        let remaining = str.[1..]
        (true, remaining)
    else
        (false, str)

let inputABC = "ABC"
let inputZBC = "ZBC"
a_parser inputABC |> printfn "%A"
a_parser inputZBC |> printfn "%A"

//Parsing a specified char and returning an explanation with the result
let pchar (charToMatch, str) =
    if String.IsNullOrEmpty(str) then
        let msg = "No more input"
        (msg, "")
    else 
        let first = str.[0] 
        if first = charToMatch then
            let remaining = str.[1..]
            let msg = sprintf "Found %c" charToMatch
            (msg, remaining)
        else 
            let msg = sprintf "Expecting '%c' but got '%c' instead" charToMatch first
            (msg, str)

pchar ('A', inputABC) |> printfn "%A"
pchar ('A', inputZBC) |> printfn "%A"

//Returning a Success or Fail
type Result<'a> =
    | Success of 'a
    | Fail of string

let pchar2 (charToMatch, str) =
    if String.IsNullOrEmpty(str) then
        Fail "No more input"
    else 
        let first = str.[0]
        if first = charToMatch then
            let remaining = str.[1..]
            Success (charToMatch, remaining)
        else
            let msg = sprintf "Expecting '%c' but got '%c' " charToMatch first
            Fail msg
pchar2 ('A', inputABC) |> printfn "%A"
pchar2 ('Z', inputABC) |> printfn "%A"
pchar2 (' ', inputABC) |> printfn "%A" 

// Switching to a curried implementation
let pchar3 charToMatch =
    let innerFn str = 
        if String.IsNullOrEmpty(str) then
            Fail "No more inputs"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Success (charToMatch, remaining)
            else
                let msg = sprintf "Expecting '%c', got '%c' " charToMatch first
                Fail msg
    innerFn
pchar3 'A' inputABC |> printfn "%A"
(*The main advantage to using currying in idiomatic F# is that
we can partially apply the character we want to parse and get
the same desired results. *)
let parseA = pchar3 'A'
parseA inputABC |> printfn "%A"


//Encapsulating the parser function in a type

type Parser<'T> = Parser of (string -> Result<'T * string>)//the value of parseA

let pchar4 charToMatch  =
    let innerFn str = 
        if String.IsNullOrEmpty(str) then
            Fail "No more inputs"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Success (charToMatch, remaining)
            else
                let msg = sprintf "Expecting '%c', got '%c' " charToMatch first
                Fail msg
    Parser innerFn //Returning our parser function wrapped in the Parser type

(*Now our parser function is no longer directly accessible because we wrapped
it in the Parser data structure, so we need a helper function that can 
extract the inner function - innerFn - and run it against the input stream.
We'll call our helper function `run`. *)
let run parser input = 
    //Unwrap parser to get inner function
    let (Parser innerFn) = parser
    innerFn input

let newParseA = pchar4 'A' 
printfn "We now try our `run` helper function on our function wrapped in a type:"
run newParseA inputABC |> printfn "%A\n"


//Combining two parsers in a sequence: the 'and then' combinator
let parse_a = pchar4 'A'
let parse_b = pchar4 'B'
(*The following gives a compiler error since you cannot
directly do function composition like you can with normal functions

`let parseAthenB = parse_a >> parse_b` 

won't work
*)
let andThen parser1 parser2 =
    let innerFn input =
        // run parser1 with the input
        let result1 = run parser1 input
        
        // test the result for Failure/Success
        match result1 with
        | Fail err -> 
            // return error from parser1
            Fail err  

        | Success (value1,remaining1) -> 
            // run parser2 with the remaining input
            let result2 =  run parser2 remaining1
            
            // test the result for Failure/Success
            match result2 with 
            | Fail err ->
                // return error from parser2 
                Fail err 
            
            | Success (value2,remaining2) -> 
                // combine both values as a pair
                let newValue = (value1,value2)
                // return remaining input after parser2
                Success (newValue,remaining2)

    // return the inner function
    Parser innerFn 
//Here we define an infix version of `andThen` so that we can use it like regular >> composition:
let ( .>>. ) = andThen

let parseAThenB = parse_a .>>. parse_b

printfn "We now test our 'and then' combinator:\n"
run parseAThenB "ABC" |> printfn "%A"
run parseAThenB "ZBC" |> printfn "%A"
run parseAThenB "AZC" |> printfn "%A"

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

