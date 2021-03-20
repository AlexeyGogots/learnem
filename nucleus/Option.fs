namespace global

open System

/// Functions for Option type (functor and monad).
[<RequireQualifiedAccess>]
module Option =

    /// Apply an Option<fn> to an Option<x> monadically
    let apply fR xR =
        match fR, xR with
        | Some f, Some x -> Some (f x)
        | _ -> None

    let private (<!>) = Option.map
    let private (<*>) = apply

    /// Transforms the result of a function following the C# `Try...(input, out output)` style
    /// into an Option
    let fromTryResult (isSuccess, value)  =
        match isSuccess with
        | true -> Some value
        | false -> None

    /// Combine a list of options, monadically
    let sequence aListOfOptions =
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Some [] // empty list inside Result

        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR aListOfOptions initialValue

    /// Combine a pair of options, monadically
    let sequenceTuple2 = function
        | (Some a, Some b) -> Some (a, b)
        | _  -> None

    /// Combine a triple of options, monadically
    let sequenceTuple3 = function
        | (Some a, Some b, Some c) -> Some (a, b, c)
        | _  -> None

    /// Gets the value of the option if the option is Some, otherwise
    /// executes the specified default value generating function.
    let defaultValueWith f = function
        | Some x -> x
        | None -> f ()

    /// Convert the None case into false, the Some case into true
    let toBoolean =
        function
        | Some _ -> true
        | None -> false

    /// Takes a function and executes it with the Ok value. Does nothing in the
    /// error case.
    let tap f =
        function
        | (Some x) as r -> f x |> ignore; r
        | None as r -> r

    let lift f x1 = Some f <*> x1
    let lift2 f x1 x2 = f <!> x1 <*> x2
    let lift3 f x1 x2 x3 = f <!> x1 <*> x2 <*> x3
    let lift4 f x1 x2 x3 x4 = f <!> x1 <*> x2 <*> x3 <*> x4
