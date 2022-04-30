module OpilioCraft.Lisp.Evaluator

// LISP evaluator: the golden heart of LISP ;-)
let rec evalExpression (env : Environment) (expr : Expression) =
    match expr with
    // quote prevents evaluation of the following expression
    | QuotedExpression expr -> expr

    // an atom evaluates to itself
    | Atom _ as atom -> atom

    // currently no support for symbol tables: a symbol evaluates to itself
    | Symbol _ as symbol -> symbol

    // do we have a function call?
    // please be aware that functions are responsible to evaluate args by themselves
    | List ( (Symbol name) :: args ) -> (env.LookupFunction name) env args |> evalExpression env

    // lists elemts are evaluated; is some kind of duplicated code according to the pattern above, but easier to read
    | List items -> items |> List.map (evalExpression env) |> List
        // TODO: do we really need this case?