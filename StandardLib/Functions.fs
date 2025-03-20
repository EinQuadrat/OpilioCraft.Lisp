module internal OpilioCraft.Lisp.StandardLib.Functions

open OpilioCraft.Lisp
open SpecialSymbols

// (and expr1 ... exprN) := expr1 [ && ... && exprN ]
let funcAnd env =
    List.fold (
        fun (result : bool) (expr : Expression) ->
            if result = true // can shortcut
            then
                Evaluator.evalExpression env expr |> function
                    | LispBoolean boolValue -> boolValue
                    | _ -> raise <| InvalidLispExpressionException "and: all arguments have to be boolean"
            else
                false
        ) true // AND: has to be true to enforce evaluation of first expression
    >> LispBoolean

// (or expr1 ... exprN) := expr1 [ || ... || exprN ]
let funcOr env =
    List.fold (
        fun (result : bool) (expr : Expression) ->
            if result = false // can shortcut
            then
                Evaluator.evalExpression env expr |> function
                    | LispBoolean boolValue -> boolValue
                    | _ -> raise <| InvalidLispExpressionException "or: all arguments have to be boolean"
            else
                true
        ) false // OR: has to be false to enforce evaluation of first expression
    >> LispBoolean

// (cond (test1 action1) [ ... (testN actionN) ] )
let rec funcCond env (cases : Expression list) : Expression =
    match cases with
    | [] -> NIL
    | List [ test ; action ] :: tail ->
        Evaluator.evalExpression env test
        |> function
            | LispBoolean true -> Evaluator.evalExpression env action
            | LispBoolean false -> funcCond env tail
            | _ -> raise <| InvalidLispExpressionException "cond( (test action) ...): test has to result into a boolean value"

    | _ -> raise <| InvalidLispExpressionException "cond expects pairs of test and action as arguments"
