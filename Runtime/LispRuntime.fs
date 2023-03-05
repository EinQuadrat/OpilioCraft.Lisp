namespace OpilioCraft.Lisp

open System

open OpilioCraft.FSharp.Prelude
open OpilioCraft.Lisp.Runtime.ObjectPathExtension

type LispRuntime private () =
    inherit MinimalRuntime ()

    // initialize ObjectPath context
    let mutable objectPathContext : ObjectPathContext =
        {
            Runtime = new OpilioCraft.ObjectPath.DefaultRuntime () :> OpilioCraft.ObjectPath.IRuntime
            ObjectData = "#UNDEFINED" :> obj
            ResultHook = id
        }

    // static initialization
    static member Initialize () =
        let instance = new LispRuntime ()

        // add standard library
        OpilioCraft.Lisp.StandardLib.unaryFunctions    |> Map.iter (fun name body -> instance.Register(name, body))
        OpilioCraft.Lisp.StandardLib.binaryFunctions   |> Map.iter (fun name body -> instance.Register(name, body))
        OpilioCraft.Lisp.StandardLib.ordinaryFunctions |> Map.iter (fun name body -> instance.Register(name, body))

        // ObjectPath extension
        instance.Register("property"           , funcLookupObjectPath instance.ObjectPathContextProvider)
        instance.Register("has-property"       , funcIsValidObjectPath instance.ObjectPathContextProvider)

        instance.Register(":property-is"       , macroPropertyIs)
        instance.Register(":property-is-not"   , macroPropertyIsNot)
        instance.Register(":property-contains" , macroPropertyContains)
        instance.Register(":property-matches"  , macroPropertyMatches)

        // return it
        instance

    // simplify function registration
    member x.Register(name, body : Function)       = body |> x.RegisterFunction name
    member x.Register(name, body : UnaryFunction)  = body |> FunctionHelper.liftUnary name  |> x.RegisterFunction name
    member x.Register(name, body : BinaryFunction) = body |> FunctionHelper.liftBinary name |> x.RegisterFunction name

    // ObjectPath context
    member private _.ObjectPathContextProvider () = objectPathContext
    member x.InjectResultHook hook = objectPathContext    <- { objectPathContext with ResultHook = hook }    ; x
    member x.InjectObjectData objData = objectPathContext <- { objectPathContext with ObjectData = objData } ; x

    // High-level API
    member _.LoadFile path =
        if IO.File.Exists path
        then
            IO.File.ReadAllText path
        else
            raise <| new IO.FileNotFoundException (null, path)

    member x.TryParse = x.ParseWithResult >> Result.toOption
    member x.TryEval = x.EvalWithResult >> Result.toOption
    member x.TryRun = x.RunWithResult >> Result.toOption

    member _.ResultToString (result : Result<Expression, string>) =
        match result with
        | Ok result -> result.ToString()
        | Error errorMsg -> $"Error occurred: {errorMsg}"

    member x.PrintResult (result : Result<Expression, string>) =
        x.ResultToString result
        |> Console.WriteLine
