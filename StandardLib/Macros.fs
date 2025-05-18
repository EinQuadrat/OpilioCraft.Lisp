module internal OpilioCraft.Lisp.StandardLib.Macros

open OpilioCraft.Lisp

// NOTE: macros rewrite the provided LISP expression list, they do not evaluate anything

let macroNotEqual _ (exprList: LispExpression list) : LispExpression =
    List [ Symbol "not" ; List ( Symbol "eq" :: exprList ) ]

let macroBetween _ (exprList: LispExpression list) : LispExpression =
    match exprList with
    | [ lower; upper; value ] ->
        let checkLowerBorder = List [ Symbol "ge"; value ; lower ]
        let checkUpperBorder = List [ Symbol "le"; value; upper ]
        List [ Symbol "and" ; checkLowerBorder ; checkUpperBorder ]
    | _ -> raise <| InvalidLispExpressionException($"between expects exactly two argument")

let macroInside _ (exprList: LispExpression list) : LispExpression =
    match exprList with
    | [ lower; upper; value ] ->
        let checkLowerBorder = List [ Symbol "gt"; value ; lower ]
        let checkUpperBorder = List [ Symbol "lt"; value; upper ]
        List [ Symbol "and" ; checkLowerBorder ; checkUpperBorder ]
    | _ -> raise <| InvalidLispExpressionException($"inside expects exactly two argument")

let macroIfThenElse _ (exprList: LispExpression list) : LispExpression =
    match exprList with
    | [ cond ; ifTrue ; otherwise ] ->
        List [ Symbol "cond" ; List [ cond ; ifTrue ] ; List [ LispBoolean true ; otherwise ]]
    | [ cond ; ifTrue ] ->
        List [ Symbol "cond" ; List [ cond ; ifTrue ] ] // if no case is true, cond returns NIL
    | _ -> raise <| InvalidLispExpressionException($"syntax if: (if cond ifTrue [otherwise])")
