namespace global

open System

/// Functions for Result type (functor and monad).
/// For applicatives, see Validation.
[<RequireQualifiedAccess>]
module Result =

    /// Pass in a function to handle each case of `Result`
    let bimap onSuccess onError r =
        match r with
        | Ok x -> onSuccess x
        | Error err -> onError err

    /// Apply a Result<fn> to a Result<x> monadically
    let apply fR xR =
        match fR, xR with
        | Ok f, Ok x -> Ok (f x)
        | Error err1, Ok _ -> Error err1
        | Ok _, Error err2 -> Error err2
        | Error err1, Error _ -> Error err1

    /// The `map`, `mapError` and `bind` functions are in a different module in F# 4.1 and newer (from VS2017),
    /// so these aliases make them available in this module.
    let map = Result.map
    let mapError = Result.mapError
    let bind = Result.bind

    let bindError fR xR =
        match xR with
        | Ok x -> Ok x
        | Error o -> fR o

    let private (<!>) = map
    let private (<*>) = apply

    /// Like `map` but with a unit-returning function
    let iter (f : _ -> unit) result =
        map f result |> ignore

    /// Like `mapError` but with a unit-returning function
    let iterError (f : _ -> unit) result =
        mapError f result |> ignore

    /// Combine a list of results, monadically
    let sequence aListOfResults =
        let cons head tail = head::tail
        let consR headR tailR = cons <!> headR <*> tailR
        let initialValue = Ok [] // empty list inside Result

        // loop through the list, prepending each element
        // to the initial value
        List.foldBack consR aListOfResults initialValue

    let sequenceOption = function
        | Some (Ok v) -> Ok (Some v)
        | Some (Error e) -> Error e
        | None -> Ok None

    /// Partitions the list of results into a list of successes and a list of errors
    let partition rs =
        let folder r (successes, errors) =
            match r with
            | Ok s -> (s::successes, errors)
            | Error e -> (successes, e::errors)

        List.foldBack folder rs ([], [])

    /// Returns r if it is Ok, otherwise returns rRecover
    let orElse rRecover r =
        match r with
        | Ok v -> Ok v
        | Error _ -> rRecover

    /// Returns r if it is Ok, otherwise evaluates fRecover and returns the result
    let orElseWith fRecover result =
        match result with
        | Ok v -> Ok v
        | Error e -> fRecover e

    //////////////////////////////////
    // Mixing simple values and results

    /// On success, return the value. On error, return a default value
    let ifError defaultVal =
        function
        | Ok x -> x
        | Error _ -> defaultVal

    /// On success, return the value. On error, return a the value returned by f
    let ifErrorWith f =
        function
        | Ok x -> x
        | Error e -> f e

    /// Convert the Error case into false, the Ok case into true
    let toBoolean =
        function
        | Ok _ -> true
        | Error e -> false

    //////////////////////////////////
    // Lifting

    /// Lift a two parameter function to use Result parameters
    let lift2 f x1 x2 =
        f <!> x1 <*> x2

    /// Lift a three parameter function to use Result parameters
    let lift3 f x1 x2 x3 =
        f <!> x1 <*> x2 <*> x3

    /// Lift a four parameter function to use Result parameters
    let lift4 f x1 x2 x3 x4 =
        f <!> x1 <*> x2 <*> x3 <*> x4

    /// Apply a monadic function with two parameters
    let bind2 f x1 x2 = lift2 f x1 x2 |> bind id

    /// Apply a monadic function with three parameters
    let bind3 f x1 x2 x3 = lift3 f x1 x2 x3 |> bind id

    //////////////////////////////////
    // Mixing options and results

    /// Apply a monadic function to an Result<x option>
    let bindOption f xR =
        match xR with
        | Some x -> f x |> map Some
        | None -> Ok None

    /// Convert an Option into a Result. If none, use the passed-in errorValue
    let ofOption errorValue opt =
        match opt with
        | Some v -> Ok v
        | None -> Error errorValue

    /// Convert an Option into a Result. If none, use the passed-in factory function
    let ofOptionWith f opt =
        match opt with
        | Some v -> Ok v
        | None -> Error (f())

    /// Convert a Result into an Option
    let toOption xR =
        match xR with
        | Ok v -> Some v
        | Error _ -> None

    /// Convert the Error case into an Option
    /// (useful with List.choose to find all errors in a list of Results)
    let toErrorOption =
        function
        | Ok _ -> None
        | Error err -> Some err

    /// Convert the error option into a result, with the given Ok case
    let ofErrorOption okValue opt =
        match opt with
        | None -> Ok okValue
        | Some e -> Error e

    //////////////////////////////////
    // Side effects

    /// Takes a function and executes it with the Ok value. Does nothing in the
    /// error case.
    let tap f =
        function
        | (Ok x) as r -> f x |> ignore; r
        | (Error e) as r -> r

    //////////////////////////////////
    // Predicates

    /// True if the error value converted to string contains the given substring
    let errorContainsString expected r =
        let contains (expected:string) (s:string) = s.ToLowerInvariant().Contains(expected)
        r |> mapError (fun e -> e.ToString()) |> toErrorOption |> Option.defaultValue "" |> contains expected

    /// Predicate that returns true on success
    let isOk =
        function
        | Ok _ -> true
        | Error _ -> false

    /// Predicate that returns true on failure
    let isError xR =
        xR |> isOk |> not

    /// Lift a given predicate into a predicate that works on Results
    let filter pred =
        function
        | Ok x -> pred x
        | Error _ -> true


[<AutoOpen>]
module ResultComputationExpression =

    type ResultBuilder() =
        member _.Return(x) = Ok x
        member _.Bind(x, f) = Result.bind f x

        member _.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member _.Delay(f) = f
        member _.Run(f) = f()

        member this.While(guard, body) =
            if not (guard())
            then this.Zero()
            else this.Bind( body(), fun () ->
                this.While(guard, body))

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation()

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () ->
                match disposable with
                    | null -> ()
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum ->
                this.While(enum.MoveNext,
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) =
            this.Bind(a, fun () -> b())

    let result = new ResultBuilder()
