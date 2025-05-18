module OpilioCraft.Lisp.Evaluator

// LISP evaluator: the golden heart of LISP ;-)
let rec evalExpression (env: Environment) (expr: Expression) =
    match expr with
    // quote prevents evaluation of the following expression
    | QuotedExpression expr -> expr

    // an atom evaluates to itself
    | Atom _ as atom -> atom

    // convention: a symbol starting with ':' is treated as a keyword and evaluates to itself
    | Symbol name as symbol when name.StartsWith(':') -> symbol

    // do we have a function call?
    | List ((Symbol name) :: args) ->
        let argsEvalBehaviour = if env.IsSpecial name then id else List.map (evalExpression env)
            // a special function gets args evaluation delegated

        args
        |> argsEvalBehaviour
        |> env.LookupFunction(name) env
        |> evalExpression env

    // lists elemts are evaluated; is some kind of duplicated code according to the pattern above, but easier to read
    | List items -> items |> List.map (evalExpression env) |> List
        // TODO: do we really need this case?

    // semantic error
    | _ -> raise <| InvalidLispExpressionException(expr.ToString())
