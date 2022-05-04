namespace OpilioCraft.Lisp

open FParsec

type MinimalRuntime () =
    // environment
    let mutable environment = Environment.empty
    let specialFunctionPrefix = ':'

    member _.ClearEnvironment () =
        environment <- Environment.empty

    member _.RegisterFunction (name : string) (body : Function) =
        let (funcName, isSpecial) =
            if name.StartsWith(specialFunctionPrefix) && name.Length > 1
            then
                name.Substring(1), true
            else
                name, false
        
        let specials =
            if isSpecial
            then
                environment.SpecialFunctions |> Set.add funcName
            else
                environment.SpecialFunctions

        environment <- { environment with
                            Functions = environment.Functions |> Map.add funcName body
                            SpecialFunctions = specials }

    // runtime behaviour
    member _.Parse lispSource =
        let parsedLisp = run OpilioCraft.Lisp.Parser.pExpression lispSource in

        match parsedLisp with
        | Success(lispExpr, _, _) -> lispExpr
        | Failure(errorMsg, _, _) -> raise <| InvalidLispExpressionException errorMsg

    member _.Eval = Evaluator.evalExpression environment
    member x.Run = x.Parse >> x.Eval

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

    member x.RunWithResult = x.ParseWithResult >> (Result.bind x.EvalWithResult)
