namespace OpilioCraft.Lisp

open FParsec

type MinimalRuntime () =
    // function support
    let mutable functionTable : Map<string, Function> = Map.empty

    let lookupFunction name =
        if functionTable.ContainsKey(name)
        then
            functionTable.[name]
        else
            raise <| UndefinedFunctionException name

    // LISP evaluator: the golden heart of LISP ;-)
    let rec evalExpression expr =
        match expr with
        // quote prevents evaluation of the following expression
        | QuotedExpression expr -> expr

        // an atom evaluates to itself
        | Atom _ as atom -> atom

        // currently no support for symbol tables: a symbol evaluates to itself
        | Symbol _ as symbol -> symbol

        // do we have a function call?
        // please be aware that functions use deferred expression evaluation to be able to shortcut evaluation e.g. for conditionals
        | List ( (Symbol symbol) :: args ) ->
            args |> List.map (fun arg -> fun _ -> evalExpression arg) |> (lookupFunction symbol) |> evalExpression

        // lists elemts are evaluated; is some kind of duplicated code according to the pattern above, but easier to read
        | List items -> items |> List.map evalExpression |> List

    // runtime behaviour
    member _.RegisterFunction name body =
        functionTable <- functionTable |> Map.add name body

    member _.Parse lispSource =
        let parsedLisp = run OpilioCraft.Lisp.Parser.pExpression lispSource in

        match parsedLisp with
        | Success(lispExpr, _, _) -> lispExpr
        | Failure(errorMsg, _, _) -> raise <| InvalidLispExpressionException errorMsg

    member _.Eval lispExpr =
        evalExpression lispExpr

    member x.Run lispSource =
        lispSource |> x.Parse |> x.Eval

    // explicit support of Result style
    member _.ParseWithResult lispSource =
        try
            let parsedLisp = run OpilioCraft.Lisp.Parser.pExpression lispSource in

            match parsedLisp with
            | Success(lispExpr, _, _) -> Result.Ok lispExpr
            | Failure(errorMsg, _, _) -> Result.Error errorMsg
        with
            | exn -> Result.Error $"{exn.ToString()}"

    member _.EvalWithResult lispExpr =
        try
            evalExpression lispExpr |> Result.Ok
        with
            | exn -> Result.Error $"{exn.ToString()}"

    member x.RunWithResult lispSource =
        lispSource |> x.ParseWithResult |> Result.bind x.EvalWithResult
