namespace global  // note use of GLOBAL namespace

open System

type AsyncResult<'Success,'Failure> =
    Async<Result<'Success,'Failure>>

[<RequireQualifiedAccess>]
module AsyncResult =

    /// Pass in a function to handle each case of `AsyncResult`
    let bimap onSuccess onError x =
        Async.map (Result.bimap onSuccess onError) x

    /// Transforms an AsyncResult value by using a specified mapping function.
    let map f (x:AsyncResult<_,_>) : AsyncResult<_,_> =
        Async.map (Result.map f) x

    /// Transforms an AsyncResult error value by using a specified mapping function.
    let mapError f (x:AsyncResult<_,_>) : AsyncResult<_,_> =
        Async.map (Result.mapError f) x

    /// Apply ignore to the internal value
    let ignore x =
        x |> map ignore

    /// Lift a value to AsyncResult
    let retn x : AsyncResult<_,_> =
        x |> Result.Ok |> Async.retn

    /// Handles asynchronous exceptions and maps them into Failure cases using the provided function
    let catch f (x:AsyncResult<_,_>) : AsyncResult<_,_> =
        x
        |> Async.Catch
        |> Async.map(function
            | Choice1Of2 (Ok v) -> Ok v
            | Choice1Of2 (Error err) -> Error err
            | Choice2Of2 ex -> Error (f ex))

    /// Apply an AsyncResult function to an AsyncResult value, monadically
    let applyM (fAsyncResult : AsyncResult<_, _>) (xAsyncResult : AsyncResult<_, _>) :AsyncResult<_,_> =
        fAsyncResult |> Async.bind (fun fResult ->
        xAsyncResult |> Async.map (fun xResult -> Result.apply fResult xResult))

    /// Apply an AsyncResult function to an AsyncResult value, applicatively
    let applyA (fAsyncResult : AsyncResult<_, _>) (xAsyncResult : AsyncResult<_, _>) :AsyncResult<_,_> =
        fAsyncResult |> Async.bind (fun fResult ->
        xAsyncResult |> Async.map (fun xResult -> Validation.apply fResult xResult))

    /// Apply a monadic function to an AsyncResult value
    let bind (f: 'a -> AsyncResult<'b,'c>) (xAsyncResult : AsyncResult<_, _>) :AsyncResult<_,_> = async {
        let! xResult = xAsyncResult
        match xResult with
            | Ok x -> return! f x
            | Error err -> return (Error err)
        }

    /// Apply a monadic function to an AsyncResult error value
    let bindError (f: 'e1 -> AsyncResult<'a,'e2>) (xAsyncResult : AsyncResult<_, _>) :AsyncResult<_,_> = async {
        let! xResult = xAsyncResult
        match xResult with
            | Ok x -> return (Ok x)
            | Error err -> return! f err
        }

    // Like `map` but with a unit-returning function
    let iter f (result:AsyncResult<_,_>) =
        Async.map (Result.iter f) result

    // Like `mapError` but with a unit-returning function
    let iterError f (result:AsyncResult<_,_>) =
        Async.map (Result.iterError f) result

    /// Convert a list of AsyncResult into a AsyncResult<list> using monadic style.
    /// Only the first error is returned. The error type need not be a list.
    let sequenceM resultList =
        let (<*>) = applyM
        let (<!>) = map
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = retn [] // empty list inside Result

        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR resultList  initialValue

    /// Convert a list of AsyncResult into a AsyncResult<list> using applicative style.
    /// All the errors are returned. The error type must be a list.
    let sequenceA resultList =
        let (<*>) = applyA
        let (<!>) = map
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = retn [] // empty list inside Result

        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR resultList  initialValue

    //////////////////////////////////
    // Mixing simple values and results

    /// On success, return the value. On error, return a default value
    let ifError defaultVal =
        Async.map (Result.ifError defaultVal)

    /// On success, return the value. On error, return a the value returned by f
    let ifErrorWith f =
        Async.map (Result.ifErrorWith f)

    //////////////////////////////////
    // Converting between AsyncResults and other types

    /// Lift a value into an Ok inside a AsyncResult
    let ofOk x : AsyncResult<_,_> =
        x |> Result.Ok |> Async.retn

    /// Lift a value into an Error inside a AsyncResult
    let ofError x : AsyncResult<_,_> =
        x |> Result.Error |> Async.retn

    /// Lift a Result into an AsyncResult
    let ofResult x : AsyncResult<_,_> =
        x |> Async.retn

    /// Lift a Async into an AsyncResult
    let ofAsync x : AsyncResult<_,_> =
        x |> Async.map Result.Ok

    /// Lift an Option into an AsyncResult
    let ofOption errorValue x : AsyncResult<_, _> =
        x |> Result.ofOption errorValue |> ofResult

    /// Lift an Option into an AsyncResult
    let ofOptionWith f x : AsyncResult<_, _> =
        x |> Result.ofOptionWith f |> ofResult

