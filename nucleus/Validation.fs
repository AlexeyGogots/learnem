namespace global

open System
// Computation Expression for Result

/// The `Validation` type is the same as the `Result` type but with a *list* of
/// failures rather than a single value. This allows `Validation` types to be
/// combined by combining their errors ("applicative-style")
type Validation<'Success,'Failure> = Result<'Success,'Failure list>

[<RequireQualifiedAccess>]
module Validation =

    /// Apply a Validation<fn> to a Validation<x> applicatively
    let apply (fV:Validation<_,_>) (xV:Validation<_,_>) :Validation<_,_> =
        match fV, xV with
        | Ok f, Ok x -> Ok (f x)
        | Error errs1, Ok _ -> Error errs1
        | Ok _, Error errs2 -> Error errs2
        | Error errs1, Error errs2 -> Error (errs1 @ errs2)

    let private (<*>) = apply
    let private (<!>) = Result.map

    /// Combine a list of Validation, applicatively
    let sequence (aListOfValidations:Validation<_,_> list) =
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok [] // empty list inside Result

        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR aListOfValidations initialValue

    //////////////////////////////////
    // Converting between Validations and other types

    let ofResult xR :Validation<_,_> =
        xR |> Result.mapError List.singleton

    let toResult (xV:Validation<_,_>) :Result<_,_> =
        xV
