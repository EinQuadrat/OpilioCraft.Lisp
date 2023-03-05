module internal OpilioCraft.Lisp.StandardLib.TypeConstructors

open OpilioCraft.FSharp.Prelude
open OpilioCraft.Lisp

// date/time related types
let ctrDate _ exprList =
    match exprList with
        | [ Atom (FlexibleValue.Numeral year); Atom (FlexibleValue.Numeral month); Atom (FlexibleValue.Numeral day) ] -> System.DateOnly(year, month, day)
        | [ Atom (FlexibleValue.Numeral year); Atom (FlexibleValue.Numeral month) ] -> System.DateOnly(year, month, 1)
        | [ Atom (FlexibleValue.Numeral year) ] -> System.DateOnly(year, 1, 1)
        | _ -> raise <| InvalidLispExpressionException $"invalid use of type constructor #date"
    |> LispDate

let ctrTime _ exprList =
    match exprList with
        | [ Atom (FlexibleValue.Numeral hour); Atom (FlexibleValue.Numeral minute); Atom (FlexibleValue.Numeral second) ] -> new System.TimeOnly(hour, minute, second)
        | [ Atom (FlexibleValue.Numeral hour); Atom (FlexibleValue.Numeral minute) ] -> System.TimeOnly(hour, minute)
        | [ Atom (FlexibleValue.Numeral hour) ] -> System.TimeOnly(hour)
        | _ -> raise <| InvalidLispExpressionException $"invalid use of type constructor #time"
    |> LispTime

let ctrDateTime _ exprList =
    match exprList with
        | [
            Atom (FlexibleValue.Numeral year)
            Atom (FlexibleValue.Numeral month)
            Atom (FlexibleValue.Numeral day)
            Atom (FlexibleValue.Numeral hour)
            Atom (FlexibleValue.Numeral minute)
            Atom (FlexibleValue.Numeral second)
            ] -> System.DateTime(year, month, day, hour, minute, second)
        | _ -> raise <| InvalidLispExpressionException $"invalid use of type constructor #datetime"
    |> LispDateTime
