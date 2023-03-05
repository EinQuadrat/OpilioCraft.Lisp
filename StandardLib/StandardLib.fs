[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OpilioCraft.Lisp.StandardLib

open StandardLib.TypeConstructors
open StandardLib.UnaryFunctions
open StandardLib.BinaryFunctions
open StandardLib.Functions
open StandardLib.Macros

// NOTE: function names beginning with ':' are treated as special functions

// function catalogue
let unaryFunctions : Map<string, UnaryFunction> = 
    Map.ofArray
        [|
            // quote
            ":quote" , quote

            // boolean functions
            ":not" , unaryNot

            // datetime functions
            "year"  , unaryYear
            "month" , unaryYear
            "day"   , unaryYear
        |]

let binaryFunctions : Map<string, BinaryFunction> =
    Map.ofArray
        [|
            // arithmetic
            "+" , binaryAdd
            "-" , binarySubtract
            "*" , binaryMultiply
            "/" , binaryDivide

            // comparison
            "eq" , binaryEqual
            "lt" , binaryLower
            "le" , binaryLowerEqual
            "gt" , binaryGreater
            "ge" , binaryGreaterEqual

            // matching
            "contains" , binaryContains
            "matches"  , binaryMatchesRegex
        |]

let ordinaryFunctions : Map<string, OrdinaryFunction> =
    Map.ofArray
        [|
            // type constructors
            "#date"     , ctrDate
            "#time"     , ctrTime
            "#datetime" , ctrDateTime

            // boolean functions
            ":and", funcAnd
            ":or", funcOr

            // other functions
            ":cond", funcCond

            // macros
            ":neq", macroNotEqual
            ":between", macroBetween
            ":inside", macroInside
            ":if", macroIfThenElse
        |]
