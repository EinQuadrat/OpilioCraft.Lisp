module OpilioCraft.Lisp.Test

// test data
let testObj = {|
    Name = "TestObject"
    Env = Map.ofList [ "SlotA", ("ValueA" :> obj) ; "SlotB", (42 :> obj) ]
|}

[<EntryPoint>]
let main _ =
    let runtime = LispRuntime.Initialize ()

    //""" (property-matches "$.Name" "est.*ct") """
    """ (property-is "$.Env.[SlotB]" 42) """
    |> runtime.InjectObjectData(testObj).RunWithResult
    |> runtime.PrintResult

    0
