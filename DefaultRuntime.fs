namespace OpilioCraft.Lisp

open FParsec

type DefaultRuntime (funcTable) =
    let mutable functionTable : Map<string, Function> = funcTable
    let mutable macroTable : Map<string, Macro> = Map.empty

    // macro handling
    let rec applyMacros (expr : Expression) : Expression =
        match expr with
        | List ( (Symbol macroName) :: tail ) when macroTable.ContainsKey(macroName) ->
            tail |> macroTable.[macroName] |> applyMacros // support nested macros
        | List elems when elems.Length > 0 ->
            elems |> List.map applyMacros |> List
        | passThrough -> passThrough

    let lookupFunction name =
        if functionTable.ContainsKey(name)
        then
            functionTable.[name]
        else
            raise <| UndefinedFunctionException name

    // constructors
    new () = DefaultRuntime Map.empty

    // private members
    member private thisRuntime.ApplyFunction name (exprList : Expression list) =
        (lookupFunction name) thisRuntime exprList
    
    // interface helpers
    member x.IRuntime = x :> IRuntime
    member x.RegisterFunction = x.IRuntime.RegisterFunction
    member x.RegisterMacro = x.IRuntime.RegisterMacro
    member x.Parse = x.IRuntime.Parse
    member x.TryParse = x.IRuntime.TryParse
    member x.Eval = x.IRuntime.Eval

    // behaviour
    interface IRuntime with
        // functions
        member _.RegisterFunction name body =
            functionTable <- functionTable |> Map.add name body

        // macros
        member _.RegisterMacro name body =
            macroTable <- macroTable |> Map.add name body

        // parsing
        member _.Parse lispString =
            let parsedLisp = run OpilioCraft.Lisp.Parser.pExpression lispString in

            match parsedLisp with
            | Success(lispExpr, _, _) -> lispExpr
            | Failure(errorMsg, _, _) -> raise <| InvalidLispExpressionException errorMsg
            |> applyMacros

        member x.TryParse lispString =
            try
                x.Parse lispString |> Some
            with
            | _ -> None

        // eval
        member x.Eval expr =
            let rec evalExpression expr =
                match expr with
                | Atom _ as atom -> atom        // an atom evaluates to itself
                | QuotedExpression expr -> expr // quote prevents evaluation of the following expression

                // currently no support for symbol tables
                | Symbol _ as symbol -> symbol

                // do we have a function call?
                | List ( (Symbol symbol) :: args ) -> args |> List.map evalExpression |> x.ApplyFunction symbol

                // lists elemts are evaluated; is some kind of duplicated code according to the pattern above, but easier to read
                | List items -> items |> List.map evalExpression |> List

            evalExpression expr
