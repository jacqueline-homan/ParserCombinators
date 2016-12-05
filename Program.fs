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
let inputEDI = "*~"
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
pchar ('~', inputEDI) |> printfn "%A"
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
printfn "\n"

//Choosing between 2 parsers: the "orElse" combinator

(*Logic implementation:
1. Run then first Parser
2. On Success, return the parsed ValueType along with then remaining inputABC
3. Otherwise, on Fail, run then second Parser with then original input,
and in this case, return the Result (Success or Fail) from then second Parser.
 *)
let orElse parser1 parser2 =
    let innerFn input =
        //Run parser1 with the input
        let result1 = run parser1 input
        //Test the result for Failure/Success
        match result1 with
        | Success result -> 
            // if Success, return the original result
            result1
        | Fail err -> 
            // if Fail, run parser2 with the input
            let result2 = run parser2 input 
            // Return parser2's result
            result2
    Parser innerFn
// Define an infix operator function for `orElse`
let ( <|> ) = orElse
// Testing the orElse parser
let ParseA = pchar4 'A'
let ParseB = pchar4 'B'
let parseAorElseB = ParseA <|> ParseB

printfn "Results of the orElse combinator:\n"
run parseAorElseB "AZZ" |> printfn "%A"
run parseAorElseB "BZZ" |> printfn "%A"
run parseAorElseB "CZZ" |> printfn "%A"
printfn "\n"

(* Combining the `andThen` and `orElse` parser combinators to
build more complex ones, such as "A and then (BorC)"
Here is how to build then AandThenBorC combinator from simpler ones:
*)
let parse_A = pchar4 'A'
let parse_B = pchar4 'B'
let parse_C = pchar4 'C'
let BorElseC = parse_B <|> parse_C
let AandThenBorC = parse_A .>>. BorElseC

//and here it is in action:
printfn "Results of the AandThenBorC parser combinator:\n"
run AandThenBorC "ABZ" |> printfn "%A"
run AandThenBorC "ACZ" |> printfn "%A"
run AandThenBorC "QBZ" |> printfn "%A"
run AandThenBorC "AQZ" |> printfn "%A"

(*Choosing from a list of parsers: "choice" and "anyOf"
Suppose you want to choose from as list of parsers instead of
only 2. If we have a pairwise way of combining things, we can 
extend that concept to combining and entire list using `reduce`.
This example will fail if the input list is an empty list.
*)
let choice listOfParsers = 
    List.reduce ( <|> ) listOfParsers
//Choose any of a list of characters
let anyOf listOfChars =
    listOfChars
    |> List.map pchar4
    |> choice

(*Test it by creating a parser for any lowercase letter
and any digit character
*)
let parseLowercase =
    anyOf ['a'..'z']

let parseDigit = 
    anyOf ['0'..'9']

printfn "Testing our `choice` and `anyOf` parser combinator:"
run parseLowercase "aBC" |> printfn "%A"
run parseLowercase "ABC" |> printfn "%A"
run parseDigit "1ABC" |> printfn "%A" 
run parseDigit "9ABC" |> printfn "%A"
run parseDigit "|ABC" |> printfn "%A"
printfn "\n"

//Using map to transform the contents of a parser
//Using a map function in "parser world"
let mapP f parser = 
    let innerFn input =
        // run parser with the input
        let result = run parser input
        // test the result for Failure/Success
        match result with
        | Success (value,remaining) -> 
            // if success, return the value transformed by f
            let newValue = f value
            Success (newValue, remaining)
        | Fail err -> 
            // if failed, return the error
            Fail err
    // return the inner function
    Parser innerFn 
//defining an infix for map
let ( <!> ) = mapP
(*And in the context of parsing, we'll often want to put the mapping function 
after the parser, with the parameters flipped. 
This makes using map with the pipeline idiom much more convenient: *)
let ( |>> ) x f = mapP f x

(*With mapP available, we can revisit parseThreeDigits and turn the tuple 
into a string. *)
let parseDigit2 = anyOf ['0'..'9']

let parseThreeDigitsAsStr = 
    // create a parser that returns a tuple
    let tupleParser = 
        parseDigit .>>. parseDigit .>>. parseDigit
    // create a function that turns the tuple into a string
    let transformTuple ((c1, c2), c3) = 
        String [| c1; c2; c3 |]
    // use "map" to combine them
    mapP transformTuple tupleParser 

//Or, if you prefer a more compact implementation:
let parse3DigitsAsStr = 
    (parseDigit .>>. parseDigit .>>. parseDigit)
    |>> fun ((c1, c2), c3) -> String [| c1; c2; c3 |]
printfn "Using map in parser world:"
run parseThreeDigitsAsStr "123A" |> printfn "%A"  // Success ("123", "A")
run parse3DigitsAsStr "456B" |> printfn "%A"

//map the string into an int:
let parseThreeDigitsAsInt = 
    mapP int parseThreeDigitsAsStr
run parseThreeDigitsAsInt "123A" |> printfn "%A"

(*`apply` and `return` -- lifting functions up into "parser world"

To achieve our goal of creating a parser that matches a list of characters, 
we need two more helper functions which I will call returnP and applyP.

returnP simply transforms a normal value into a value in Parser World
applyP transforms a Parser containing a function (Parser< 'a->'b >) 
into a function in Parser World (Parser<'a> -> Parser<'b >)
*)
let returnP x =
    let innerFn input =
        // ignore the input and return x
        Success (x, input)
    // return the inner function
    Parser innerFn

//Here is the implementation of `applyP`, which uses .>>. and map:
let applyP fP xP =
    // create a parser containing a pair (f,x)
    (fP .>>. xP)
    // map the pair by applying f to x
    |> mapP (fun (f, x) -> f x)

// Defining an infix operatpr for applyP:
let (<*>) = applyP

(* Using `returnP` and `applyP` together, we can lift any function 
in "normal world" into a function in "parser world" regardless of 
how many parameters it has. Example: we can now define a `lift2`
function that will lift a 2-parameter function into "parser world" *)
let lift2 f xP yP =
    returnP f <*> xP <*> yP

//Lifting integer addition to the addition of parsers
let addP = lift2 (+)

//Lifting a bool to parser world and returning Parser<bool>
let startsWith (str:string) prefix =
    str.StartsWith(prefix)

let startsWithP = lift2 startsWith

//sequence -- transforming a list of Parsers into a single Parser

let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head::tail

    // lift it to Parser World
    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] -> 
        returnP []
    | head::tail ->
        consP head (sequence tail)

let parsers = [ pchar4 'A'; pchar4 'B'; pchar4 'C' ]
let combined = sequence parsers

run combined "ABCD" |> printfn "%A"

printfn "\n"

(* Implementing the pstring parser
At last, we can implement the parser that matches a string, 
which we'll call pstring.

The logic is:

Convert the string into a list of characters.
Convert each character into a Parser<char>.
Use sequence to convert the list of Parser<char> into a single Parser<char list>.
And finally, use map to convert the Parser<char list> into a Parser<string>.
Here's the code:
*)
/// Helper to create a string from a list of chars
let charListToStr charList = 
    String(List.toArray charList)

// match a specific string
let pstring str = 
    str
    // convert to list of char
    |> List.ofSeq
    // map each char to a pchar
    |> List.map pchar4 
    // convert to Parser<char list>
    |> sequence
    // convert Parser<char list> to Parser<string>
    |> mapP charListToStr 
//Let's test it:

let parseABC = pstring "ABC"
printfn "Implementing the pstring parser:\n"
run parseABC "ABCDE" |> printfn "%A" // Success ("ABC", "DE")
run parseABC "A|CDE" |> printfn "%A" // Failure "Expecting 'B'. Got '|'"
run parseABC "AB|DE" |> printfn "%A" // Failure "Expecting 'C'. Got '|'"

printfn "\n"

//4. many and many1 -- matching a parser multiple times
// Our helper function which we'll need to define `many`
let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = run parser input 
    // test the result for Failure/Success
    match firstResult with
    | Fail err -> 
        // if parse fails, return empty list
        ([],input)  
    | Success (firstValue,inputAfterFirstParse) -> 
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues,remainingInput) = 
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue::subsequentValues
        (values,remainingInput) 

//Defining `many` -- wrapped over `parseZeroOrMore`
let many parser = 
    let rec innerFn input = 
        //parse the input, wrap it in Success since it always succeeds
        Success(parseZeroOrMore parser input)
    Parser innerFn

// Testing `many`
let manyA = many(pchar4 'A')
//test for some success cases
run manyA "ABCD" |> printfn "%A"
run manyA "AACD" |> printfn "%A"
run manyA "AAAD" |> printfn "%A"
//test case with no matches
run manyA "|BCD" |> printfn "%A"

(*There's nothing about many that restricts its use to single characters. 
For example, we can use it to match repetitive string sequences too: *)
printfn "\n"
printfn "Using `many` on a string sequence:\n"

let manyAB = many (pstring "AB")

run manyAB "ABCD" |> printfn "%A"
run manyAB "ABABCD" |> printfn "%A"
run manyAB "ZCD" |> printfn "%A"
run manyAB "AZCD" |> printfn "%A"
printfn "\n"
//Implementing the original example of matching whitespace:
printfn "Implementing the original example of matching whitespace:\n"

let whitespaceChar = anyOf [' '; '\t'; '\n']
let whitespace = many whitespaceChar 

run whitespace "ABC" |> printfn "%A" // Success ([], "ABC")
run whitespace " ABC" |> printfn "%A"  // Success ([' '], "ABC")
run whitespace "\tABC" |> printfn "%A" // Success (['\t'], "ABC")

(*Defining many1
We can also define the "one or more" combinator many1, using the following logic:
Run the parser.
If it fails, return the failure.
If it succeeds:
Call the helper function parseZeroOrMore to get the remaining values.
Then combine the first value and the remaining values.*)

printfn "\n"
printfn "The one-or-more combinator, `many1`:\n"
// match one or more occurences of the specified parser
let many1 parser = 
    let rec innerFn input =
        // run parser with the input
        let firstResult = run parser input 
        // test the result for Failure/Success
        match firstResult with
        | Fail err -> 
            Fail err // failed
        | Success (firstValue,inputAfterFirstParse) -> 
            // if first found, look for zeroOrMore now
            let (subsequentValues,remainingInput) = 
                parseZeroOrMore parser inputAfterFirstParse
            let values = firstValue::subsequentValues
            Success (values,remainingInput)  
    Parser innerFn

// define parser for one digit
let digit = anyOf ['0'..'9']

// define parser for one or more digits
let digits = many1 digit 

run digits "1ABC" |> printfn "%A" // Success (['1'], "ABC")
run digits "12BC" |> printfn "%A"// Success (['1'; '2'], "BC")
run digits "123C" |> printfn "%A" // Success (['1'; '2'; '3'], "C")
run digits "1234" |> printfn "%A" // Success (['1'; '2'; '3'; '4'], "")
//No digits match case
run digits "ABC"  |> printfn "%A" // Failure "Expecting '9'. Got 'A'"
printfn "\n"

(*Using many1, we can create a parser for an integer. 
The implementation logic is:

- Create a parser for a digit.
- Use many1 to get a list of digits.
- Using map, transform the result (a list of digits) into a string 
and then into an int. *)
let pint = 
    // helper
    let resultToInt digitList = 
        // ignore int overflow for now
        String(List.toArray digitList) |> int
    // define parser for one digit
    let digit = anyOf ['0'..'9']
    // define parser for one or more digits
    let digits = many1 digit 
    // map the digits to an int
    digits 
    |> mapP resultToInt

printfn "Using map, transform the list of digits into a string \r" 
printfn "and then into an int:\n"

run pint "1ABC" |> printfn "%A" // Success (1, "ABC")
run pint "12BC" |> printfn "%A" // Success (12, "BC")
run pint "123C" |> printfn "%A"// Success (123, "C")
run pint "1234" |> printfn "%A" // Success (1234, "")
run pint "ABC" |> printfn "%A"  // Failure "Expecting '9'. Got 'A'"

printfn "\n"

(*We can define an opt combinator easily:

Change the result of a specified parser to an option by mapping the result to Some.
Create another parser that always returns None.
Use <|> to choose the second ("None") parser if the first fails.
*)
let opt p =
    let some = p |>> Some
    let none = returnP None
    some  <|> none 
//We've already defined digit on line #485 above
//We match a digit followed by an optional semicolon:
let digitThenSemicolon = digit .>>. opt (pchar4 ';')
printfn "Demonstrating the opt parser combinator:\n"
run digitThenSemicolon "1;"|> printfn "%A"  // Success (('1', Some ';'), "")
run digitThenSemicolon "1" |> printfn "%A"  // Success (('1', None), "")

printfn "\n"
//And here is pint rewritten to handle an optional minus sign: 
let pint2 = 
    // helper
    let resultToInt (sign,charList) = 
        let i = String(List.toArray charList) |> int
        match sign with
        | Some ch -> -i  // negate the int
        | None -> i
    // define parser for one digit
    let digit = anyOf ['0'..'9']
    // define parser for one or more digits
    let digits = many1 digit 

    // parse and convert
    opt (pchar4 '-') .>>. digits 
    |>> resultToInt 
printfn "pint rewritten to handle negative numbers with opt:\n"
run pint2 "123C" |> printfn "%A"  // Success (123, "C")
run pint2 "-123C" |> printfn "%A" // Success (-123, "C")

printfn "\n"

printfn "Keeping some results, discarding others:\n"
let (.>>) p1 p2 = 
    // create a pair
    p1 .>>. p2 
    // then only keep the first value
    |> mapP (fun (a,b) -> a) 


/// Keep only the result of the right side parser    
let (>>.) p1 p2 = 
    // create a pair
    p1 .>>. p2 
    // then only keep the second value
    |> mapP (fun (a,b) -> b)

/// Keep only the result of the left side parser
(* These combinators allow us to simplify the 
digitThenSemicolon example shown earlier:

let digit = anyOf ['0'..'9']

// use .>> below
let digitThenSemicolon = digit .>> opt (pchar ';')  

run digitThenSemicolon "1;"  // Success ('1', "")
run digitThenSemicolon "1"   // Success ('1', "")
*)
printfn "Use .>> to keep only the result of the left side parser:\n"
let digitB4Semicolon1 = digit .>> opt (pchar4 ';')
//the result now is the same, whether or not the semicolon was present
run digitB4Semicolon1 "1;" |> printfn "%A"
run digitB4Semicolon1 "1" |> printfn "%A" 

printfn "\n"
printfn "Use >>. to keep only the result of the right side parser:\n"

let digitB4Semicolon2 = digit >>. opt (pchar4 ';')

run digitB4Semicolon2 "1;" |> printfn "%A"
run digitB4Semicolon2 "1" |> printfn "%A"

printfn "\n"
(*an example with whitespace - a parser that looks for "AB" 
followed by one or more whitespace chars, followed by "CD" *)

let wsChar = anyOf [' '; '\t'; '\n']
let ws = many1 wsChar 

let ab = pstring "AB"
let cd = pstring "CD"
let ab_cd = (ab .>> ws) .>>. cd
printfn "A parser that looks for AB followed by one or more \r" 
printfn "whitespace chars, followed by CD: \n"
run ab_cd "AB \t\nCD" |> printfn "%A" // Success (("AB", "CD"), "")

printfn "\n"

(* A particularly common requirement is to look for a parser between delimiters such as quotes or brackets.

Creating a combinator for this is trivial:
*)
/// Keep only the result of the middle parser
let between p1 p2 p3 = 
    p1 >>. p2 .>> p3 

let pdoublequote = pchar4 '"'
let quotedInteger = between pdoublequote pint pdoublequote

run quotedInteger "\"1234\"" |> printfn "%A"   // Success (1234, "")
run quotedInteger "1234" |> printfn "%A"      // Fail "Expecting '"'. Got '1'"
printfn "\n"
/// Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
    let sepThenP = sep >>. p            
    p .>>. many sepThenP 
    |>> fun (p,pList) -> p::pList

/// Parses zero or more occurrences of p separated by sep
let sepBy p sep =
    sepBy1 p sep <|> returnP []

let comma = pchar4 ',' 
//using the definition for digit on line #483
let zeroOrMoreDigitList = sepBy digit comma
let oneOrMoreDigitList = sepBy1 digit comma

printfn "**Parsing between delimiters**\n"
printfn "One or more:\n"
run oneOrMoreDigitList "1;" |> printfn "%A"     // Success (['1'], ";")
run oneOrMoreDigitList "1,2;" |> printfn "%A"    // Success (['1'; '2'], ";")
run oneOrMoreDigitList "1,2,3;" |> printfn "%A"  // Success (['1'; '2'; '3'], ";")
run oneOrMoreDigitList "Z;" |> printfn "%A"      // Failure "Expecting '9'. Got 'Z'"
printfn "\n"
printfn "Zero or more:\n"
run zeroOrMoreDigitList "1;" |> printfn "%A"     // Success (['1'], ";")
run zeroOrMoreDigitList "1,2;" |> printfn "%A"  // Success (['1'; '2'], ";")
run zeroOrMoreDigitList "1,2,3;" |> printfn "%A"// Success (['1'; '2'; '3'], ";")
run zeroOrMoreDigitList "Z;" |> printfn "%A"    // Success ([], "Z;")

(*In an EDI text file, we see there are tildes and stars.
What if we want to parse then tildes and/or stars? 

What if we try the same thing with those as Web did with digits?

// define parser for one digit
let digit = anyOf ['0'..'9']

// define parser for one or more digits
let digits = many1 digit 

*)

//Define parser for one non-alphanumeric char
let starsAndTildes = anyOf ['*'; '~']


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

