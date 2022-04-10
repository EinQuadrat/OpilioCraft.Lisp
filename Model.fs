namespace OpilioCraft.Lisp

open System
open OpilioCraft.FSharp.Prelude

exception InvalidLispExpressionException of ErrorMsg:string
    with override x.ToString () = $"Invalid or unsupported LISP expression: {x.ErrorMsg}"

exception WrongNumberOfArgsException of FuncName:string * Expected:int
    with override x.ToString () = $"Function {x.FuncName} expects exactly {x.Expected} argument(s)"

exception UndefinedFunctionException of Name:string
    with override x.ToString () = $"{x.Name} is not a defined function"

exception InvalidArgsException
    with override x.ToString () = $"Invalid arguments"

exception UndefinedSymbolException of Name:string
    with override x.ToString () = $"{x.Name} is not a defined symbol"

exception UnsupportedPrimitiveType of Type:System.Type
    with override x.ToString () = $"{x.Type.Name} is not supported as primitive value type"

// AST nodes
type Expression = 
    | Atom of FlexibleValue
    | Symbol of string
    | List of Expression list
    | QuotedExpression of Expression

    override x.ToString () =
        let rec stringify = function
            | Atom primitive ->
                match primitive with
                | FlexibleValue.Boolean true -> "T"
                | FlexibleValue.Boolean false -> "NIL"
                | FlexibleValue.Numeral numValue -> $"{numValue}"
                | FlexibleValue.Decimal numValue -> $"{numValue}"
                | FlexibleValue.String stringValue -> $"\"{stringValue}\""
                | FlexibleValue.Date dateValue -> dateValue.ToString("yyyy-MM-dd")
                | FlexibleValue.Time timeValue -> timeValue.ToString("hh:mm:ss")
                | FlexibleValue.DateTime dateTimeValue -> dateTimeValue.ToString("yyyy-MM-ddThh:mm:ss")
                | unsupported -> raise <| UnsupportedPrimitiveType (unsupported.GetType())

            | Symbol symbol -> symbol
    
            | List [] -> "NIL"
            | List listValue ->
                let listAsString = listValue |> List.map stringify |> (fun strings -> String.Join(" ", strings)) in
                $"({listAsString})"
    
            | QuotedExpression expr -> "'" + (stringify expr)
            
        stringify x

// Function types
and UnaryOperator = IRuntime -> Expression -> Expression
and BinaryOperator = IRuntime -> Expression * Expression -> Expression
and Function = IRuntime -> Expression list -> Expression
and Macro = Expression list -> Expression

// Runtime
and IRuntime =
    abstract member RegisterFunction : string -> Function -> unit
    abstract member RegisterMacro : string -> Macro -> unit

    abstract member Parse : string -> Expression // should apply all registered macros after parsing
    abstract member TryParse : string -> Expression option

    abstract member Eval : Expression -> Expression


[<AutoOpen>]
module Primitives =
    let LispBoolean     = FlexibleValue.Boolean >> Atom
    let LispDecimal     = FlexibleValue.Decimal >> Atom
    let LispNumeral     = FlexibleValue.Numeral >> Atom
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
