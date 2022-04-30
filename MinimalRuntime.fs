namespace OpilioCraft.Lisp

open FParsec

type MinimalRuntime () =
    // environment
    let mutable environment = Environment.empty

    member _.ClearEnvironment () =
        environment <- Environment.empty

    member _.RegisterFunction name body =
        environment <- { environment with Functions = environment.Functions |> Map.add name body }

    // runtime behaviour
    member _.Parse lispSource =
        let parsedLisp = run OpilioCraft.Lisp.Parser.pExpression lispSource in

        match parsedLisp with
        | Success(lispExpr, _, _) -> lispExpr
        | Failure(errorMsg, _, _) -> raise <| InvalidLispExpressionException errorMsg

    member _.Eval lispExpr =
        Evaluator.evalExpression environment lispExpr

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

    member x.EvalWithResult lispExpr =
        try
            x.Eval lispExpr |> Result.Ok
        with
            | exn -> Result.Error $"{exn.ToString()}"

    member x.RunWithResult lispSource =
        lispSource |> x.ParseWithResult |> Result.bind x.EvalWithResult
