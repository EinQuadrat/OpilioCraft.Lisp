module internal OpilioCraft.Lisp.FunctionHelper

    let liftExpression (expr: LispExpression) = [ expr ]

    let liftUnary (opName: string) (op: UnaryFunction) (env: Environment) (args: LispExpression list) =
        match args with
        | [ arg ] -> op env arg
        | _ -> raise <| InvalidLispExpressionException($"Function {opName} expects exactly one argument")

    let liftBinary (opName: string) (op: BinaryFunction) (env: Environment) (args: LispExpression list) =
        match args with
        | [ a; b ] -> op env (a, b)
        | _ -> raise <| InvalidLispExpressionException($"Function {opName} expects exactly two arguments")
