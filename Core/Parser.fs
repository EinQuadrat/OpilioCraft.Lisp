module OpilioCraft.Lisp.Parser

open FParsec

// narrow types
type Parser<'t> = Parser<'t, unit>

// lexical stuff
let (|IsSpecialCharacter|_|) = function
    | '(' | ')'                 // list delimiter
    | '"'                       // string delimiter
    | '\''                      // quote operator
    | '.' | ';' as c -> Some c  // reserved to align with LISP syntax 
    | _ -> None

let (|IsWhitespace|_|) = function
    | ' ' | '\t' | '\n' | '\r' as c -> Some c
    | _ -> None

let pOpeningBracket : Parser<_> = pchar '(' .>> spaces
let pClosingBracket : Parser<_> = pchar ')' .>> spaces

// parse plain values
let pTrue : Parser<_> = stringCIReturn "t" LispTrue
let pFalse : Parser<_> = stringCIReturn "nil" LispFalse

let pNumeralOrDecimal : Parser<_> =
    let numberFormat =
        NumberLiteralOptions.AllowFraction
        ||| NumberLiteralOptions.AllowExponent
        ||| NumberLiteralOptions.AllowMinusSign
        ||| NumberLiteralOptions.AllowPlusSign

    numberLiteral numberFormat "number" |>> function
        | num when num.IsInteger -> (int num.String) |> LispNumeral
        | num -> (decimal num.String) |> LispDecimal

let pString : Parser<_> =
    let normalChar = satisfy (fun c -> c <> '"') in
    between (pchar '"') (pchar '"') (manyChars normalChar) |>> LispString

let pAtom =
    choice [
        pTrue
        pFalse
        pNumeralOrDecimal
        pString
    ] .>> spaces // skips trailing whitespace

// parse symbols
let pSymbol =
    let allowedChar = function
        | IsSpecialCharacter _ | IsWhitespace _ -> false
        | _ -> true

    many1Satisfy allowedChar .>> spaces |>> Symbol

// parse expressions
let pExpression, pExpressionImpl = createParserForwardedToRef()

let pList =
    pOpeningBracket >>. (sepEndBy pExpression spaces) .>> pClosingBracket |>> List

let pQuotedExpression =
    pchar '\'' >>. pExpression .>> spaces |>> QuotedExpression

do pExpressionImpl :=
    spaces >>. // skip leading whitespace
    choice [
        pAtom // order is important! atom has to be first to ensure e.g. numbers would not be treated as symbols
        pSymbol
        pList
        pQuotedExpression
    ]
