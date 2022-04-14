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

    // runtime behaviour
    member _.RegisterFunction name body =
        functionTable <- functionTable |> Map.add name body

    member _.Parse sourceCode =
        let parsedLisp = run OpilioCraft.Lisp.Parser.pExpression sourceCode in

        match parsedLisp with
        | Success(lispExpr, _, _) -> lispExpr
        | Failure(errorMsg, _, _) -> raise <| InvalidLispExpressionException errorMsg

    member x.Eval lispExpr =
        let rec evalExpression expr =
            match expr with
            // quote prevents evaluation of the following expression
            | QuotedExpression expr -> expr 

            // an atom evaluates to itself
            | Atom _ as atom -> atom

            // currently no support for symbol tables: a symbol evaluates to itself
            | Symbol _ as symbol -> symbol

            // do we have a function call?
            | List ( (Symbol symbol) :: args ) -> args |> List.map evalExpression |> (lookupFunction symbol)

            // lists elemts are evaluated; is some kind of duplicated code according to the pattern above, but easier to read
            | List items -> items |> List.map evalExpression |> List

        evalExpression lispExpr

    member x.Run lispString =
        lispString |> x.Parse |> x.Eval
