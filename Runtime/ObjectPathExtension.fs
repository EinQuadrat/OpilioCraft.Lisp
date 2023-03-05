module internal OpilioCraft.Lisp.Runtime.ObjectPathExtension

open OpilioCraft.FSharp.Prelude
open OpilioCraft.Lisp

// runtime context
type ObjectPathContext =
    {
        Runtime     : OpilioCraft.ObjectPath.IRuntime
        ObjectData  : obj
        ResultHook  : obj -> obj
    }

and ObjectPathContextProvider = unit -> ObjectPathContext

// main ObjectPath function
let private evalObjectPath (context : ObjectPathContext) objectPath : obj option =
    context.Runtime.ObjectData <- context.ObjectData
    context.Runtime.TryRun objectPath
    
let funcIsValidObjectPath (contextProvider : ObjectPathContextProvider) (env : Environment) (exprList : Expression list) : Expression =
    match exprList with
    | [ Atom (FlexibleValue.String objectPath) ] ->
        evalObjectPath (contextProvider()) objectPath
        |> Option.isSome
        |> FlexibleValue.Boolean
        |> Atom

    | _ -> raise <| InvalidLispExpressionException "has-property excepts exactly one argument containing an object path"

let funcLookupObjectPath (contextProvider : ObjectPathContextProvider) (env : Environment) (exprList : Expression list) : Expression =
    match exprList with
    | [ Atom (FlexibleValue.String objectPath) ] -> objectPath, (Symbol ":UNKNOWN-PROPERTY")
    | [ Atom (FlexibleValue.String objectPath) ; Atom _ ] as [ _ ; defaultValue ]-> objectPath, defaultValue
    | _ -> raise <| InvalidLispExpressionException "property excepts an object path and optionally an atom as default value"

    |> fun (objectPath, defaultValue) -> 
        let context = contextProvider() in
        
        evalObjectPath context objectPath
        |> Option.map context.ResultHook
        |> Option.bind FlexibleValue.TryWrap
        |> Option.map Atom
        |> Option.defaultValue defaultValue

// supportive macros
let macroPropertyIs _ (exprList : Expression list) : Expression =
    match exprList with
    | [ Atom (FlexibleValue.String _) ; Atom _ ] as [ objectPath ; pattern ] ->
        List [ Symbol "eq" ; List [ Symbol "property" ; objectPath ] ; pattern ]
    | _ -> raise <| InvalidLispExpressionException "property-is expects an object path and an atom as arguments"

let macroPropertyIsNot _ (exprList : Expression list) : Expression =
    match exprList with
    | [ Atom (FlexibleValue.String _); Atom _ ] as [ objectPath ; pattern ] ->
        List [ Symbol "not" ; List [ Symbol "property-is" ; objectPath ; pattern ] ]
    | _ -> raise <| InvalidLispExpressionException "property-is-not expects an object path and an atom as arguments"

let macroPropertyContains _ (exprList : Expression list) : Expression =
    match exprList with
    | [ Atom _ ; Atom _ ] as [ objectPath ; pattern ] ->
        List [ Symbol "contains" ; List [ Symbol "property" ; objectPath ] ; pattern ]
    | _ -> raise <| InvalidLispExpressionException "property-contains expects an object path and an atom as arguments"

let macroPropertyMatches _ (exprList : Expression list) : Expression =
    match exprList with
    | [ Atom _ ; Atom _ ] as [ objectPath ; pattern ] ->
        List [ Symbol "matches" ; List [ Symbol "property" ; objectPath ] ; pattern ]
    | _ -> raise <| InvalidLispExpressionException "property-matches expects an object path and an atom as arguments"
