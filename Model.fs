namespace OpilioCraft.Lisp

open OpilioCraft.FSharp.Prelude

exception InvalidLispExpressionException of ErrorMsg:string
    with override x.ToString () = $"Invalid or unsupported LISP expression: {x.ErrorMsg}"

exception UndefinedFunctionException of Name:string
    with override x.ToString () = $"{x.Name} is not a defined function"

// LISP elements
type Expression = 
    | Atom of FlexibleValue
    | Symbol of string // special case of an atom: a string not enclosed in quotation marks; simplifies evaluation
    | List of Expression list
    | QuotedExpression of Expression

    override x.ToString () =
        let rec stringify = function
            | Atom atomValue ->
                match atomValue with
                | FlexibleValue.Boolean true -> "T"
                | FlexibleValue.Boolean false -> "NIL"
                | FlexibleValue.Numeral numValue -> $"{numValue}"
                | FlexibleValue.Decimal numValue -> $"{numValue}"
                | FlexibleValue.String stringValue -> $"\"{stringValue}\""
                | FlexibleValue.Date dateValue -> dateValue.ToString("yyyy-MM-dd")
                | FlexibleValue.Time timeValue -> timeValue.ToString("hh:mm:ss")
                | FlexibleValue.DateTime datetimeValue -> datetimeValue.ToString("yyyy-MM-ddThh:mm:ss")
                | otherKindOfValue -> otherKindOfValue.ToString()

            | Symbol symbol -> symbol
    
            | List [] -> "()"
            | List listValue ->
                let listAsString = listValue |> List.map stringify |> (fun strings -> System.String.Join(" ", strings)) in
                $"({listAsString})"
    
            | QuotedExpression expr -> "'" + (stringify expr)
            
        stringify x

// LISP function
and Function = Expression list -> Expression

// ------------------------------------------------------------------------------------------------

[<AutoOpen>]
module LispAtomHelper =
    let LispBoolean     = FlexibleValue.Boolean >> Atom
    let LispNumeral     = FlexibleValue.Numeral >> Atom
    let LispDecimal     = FlexibleValue.Decimal >> Atom
    let LispString      = FlexibleValue.String >> Atom

    let LispTrue        = LispBoolean true
    let LispFalse       = LispBoolean false

    let LispDate        = FlexibleValue.Date >> Atom
    let LispTime        = FlexibleValue.Time >> Atom
    let LispDateTime    = FlexibleValue.DateTime >> Atom


[<AutoOpen>]
module LispActivePatterns =
    let (|LispBoolean|_|) = function
        | Atom (FlexibleValue.Boolean boolValue) -> Some boolValue
        | _ -> None

    let (|LispNumeral|_|) = function
        | Atom (FlexibleValue.Numeral numValue) -> Some numValue
        | _ -> None

    let (|LispDecimal|_|) = function
        | Atom (FlexibleValue.Decimal numValue) -> Some numValue
        | _ -> None

    let (|LispString|_|) = function
        | Atom (FlexibleValue.String stringValue) -> Some stringValue
        | _ -> None

    let (|LispDate|_|) = function
        | Atom (FlexibleValue.Date dateValue) -> Some dateValue
        | _ -> None

    let (|LispTime|_|) = function
        | Atom (FlexibleValue.Time timeValue) -> Some timeValue
        | _ -> None

    let (|LispDateTime|_|) = function
        | Atom (FlexibleValue.DateTime dateTimeValue) -> Some dateTimeValue
        | _ -> None
