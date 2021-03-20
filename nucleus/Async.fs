namespace global  // note use of GLOBAL namespace

open FSharp.Data.UnitSystems.SI.UnitNames
open System

/// Async utilities
[<RequireQualifiedAccess>]  // RequireQualifiedAccess forces the `Async.xxx` prefix to be used
module Async =

    /// Transforms an Async value by using a specified mapping function.
    let map f xA =
        async {
            let! x = xA
            return f x
        }

    /// Lift a value to Async
    let retn x =
        async.Return x

    /// Apply an Async function to an Async value
    let apply fA xA =
        async {
             // start the two asyncs in parallel
            let! fChild = Async.StartChild fA  // run in parallel
            let! x = xA
            // wait for the result of the first one
            let! f = fChild
            return f x
        }

    /// Apply a monadic function to an Async value
    let bind f xA = async.Bind(xA,f)

    /// Convert a collection of `Async`s into an `Async` collection by doing them
    /// in sequence. Consider using `Async.Parallel` for increased performance.
    let sequence (asyncs: seq<Async<'t>>) : Async<'t list> =
        let rec loop acc remaining =
            async {
                match remaining with
                | [] ->
                    return List.rev acc
                | x::xs ->
                    let! res = x
                    return! loop (res::acc) xs
            }
        loop [] (List.ofSeq asyncs)

    let makeCancellationTokenSourceWithTimeout (timeout:float<second>) =
        let cts = new Threading.CancellationTokenSource()
        let timeoutInMs = timeout * 1000.0 / 1.0<second> |> System.Math.Round |> int
        cts.CancelAfter(timeoutInMs)
        cts

    let startAsTask cancellationToken asyncOp =
        Async.StartAsTask(asyncOp, cancellationToken = cancellationToken)

    /// MSDN's docs state that the timeout arg is ignored when a cancellationToken is given, this
    /// function adds that functionality.
    let runSynchronouslyWithTimeout timeout cancellationToken (a:Async<'T>) =
        let timeoutCts = makeCancellationTokenSourceWithTimeout timeout
        let linkedCts = Threading.CancellationTokenSource.CreateLinkedTokenSource(cancellationToken, timeoutCts.Token)

        try
            Async.RunSynchronously(a, cancellationToken = linkedCts.Token)
        with
        :? OperationCanceledException as e when timeoutCts.IsCancellationRequested ->
            // If the timeout occurred, then we throw timeout exception instead
            raise (new System.TimeoutException())
