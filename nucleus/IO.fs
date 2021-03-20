namespace global

open System

[<NoComparison>]
[<NoEquality>]
[<Struct>]
type IO<'context, 'result, 'error> =
    IO of ('context -> AsyncResult<'result, 'error>)

[<RequireQualifiedAccess>]
module IO =
    let internal unsafeRun r (IO f) = f r

    let run error r m  =
        try
            unsafeRun r m
            |> Async.Catch
            |> Async.map (function | Choice1Of2 v -> v | Choice2Of2 e -> error e |> Error)
        with
            e -> error e |> AsyncResult.ofError

    let startAsTask ctx onSuccess onError m =
        let onException = GenericError.Exception >> onError
        unsafeRun () m
        |> AsyncResult.bimap onSuccess onError
        |> Async.Catch
        |> Async.map (function | Choice1Of2 v -> v | Choice2Of2 e -> onException e)
        |> Async.startAsTask ctx

    let run' m = run GenericError.Exception () m

    let runSynchronouslyWithTimeout timeout ctx r m =
        try
            run GenericError.Exception r m
            |> Async.runSynchronouslyWithTimeout timeout ctx // Might throw because of timeout, canceled, ...
        with
            e -> GenericError.Exception e |> Error

    let runSynchronously r m =
        try
            run GenericError.Exception r m
            |> Async.RunSynchronously // Might throw
        with
            e -> GenericError.Exception e |> Error


    let ret a = IO (fun _ -> AsyncResult.ofOk a)

    let empty = IO (fun _ -> AsyncResult.ofOk ())

    let ask = IO (fun r -> AsyncResult.ofOk r)

    let local (f:'r1->'r2) (IO m) = IO (f >> m )

    let reader (f:'r->'a) = IO (fun r -> AsyncResult.ofOk (f r))

    let map (f:'a->'b) m  =
        IO (fun r -> let x = unsafeRun r m in AsyncResult.map f x)

    let mapError (f:'e1->'e2) m =
        IO (fun r -> let x = unsafeRun r m in AsyncResult.mapError f x)

    let bind f m =
        IO (fun r -> let x = unsafeRun r m in AsyncResult.bind (fun x' -> unsafeRun r (f x')) x)

    let bindError f m =
        IO (fun r -> let x = unsafeRun r m in AsyncResult.bindError (fun x' -> unsafeRun r (f x')) x)

    let apply f m = bind (fun f' -> map f' m ) f

    let private ( <!> ) f m = map f m
    let private ( <*> ) f m = apply f m

    let private cons head tail = head :: tail

    let sequenceM a =
        let f = fun x xs -> let f = map cons x in apply f xs
        List.foldBack f a (ret [])

//    let sequenceA a =
//        let (<*>) (f:IO<_,'a -> 'b list, 'e list>) (m:IO<_, _, 'e list>) : IO<_, 'b list, 'e list> =
//            fun r ->
//                async {
//                    let! g = unsafeRun r f
//                    let! x = unsafeRun r m
//                    return Validation.apply g x
//                }
//            |> IO
//
//        let cons head tail = cons <!> head <*> tail
//        List.foldBack cons a (ret [])

    let sequenceA error a =
        fun r ->
            a
            |> List.map (run error r)
            |> Async.Parallel
            |> Async.map (Array.toList)
            |> Async.map Ok
        |> IO

    let lift f x1 = ret f <*> x1
    let lift2 f x1 x2 = f <!> x1 <*> x2
    let lift3 f x1 x2 x3 = f <!> x1 <*> x2 <*> x3
    let lift4 f x1 x2 x3 x4 = f <!> x1 <*> x2 <*> x3 <*> x4

    let bind2 f x1 x2 = lift2 f x1 x2 |> bind id
    let bind3 f x1 x2 x3 = lift3 f x1 x2 x3 |> bind id

    let ifError f m =
        IO (fun r -> let x = unsafeRun r m in AsyncResult.bimap id f x |> AsyncResult.ofAsync)

    let ofSuccess = ret
    let ofError err = IO (fun _ -> AsyncResult.ofError err)
    let ofAsync a = IO (fun _ -> Async.bind (fun x -> AsyncResult.ofOk x) a)
    let ofAsyncResult x = IO (fun _ -> x)
    let ofOption errorValue x = IO (fun _ -> AsyncResult.ofOption errorValue x)
    let ofResult x = IO (fun _ -> AsyncResult.ofResult x)


    let hoist f x1 = IO (fun _ -> AsyncResult.ofResult (f x1))
    let hoist2 f x1 x2 = IO (fun _ -> AsyncResult.ofResult (f x1 x2))
    let hoist3 f x1 x2 x3 = IO (fun _ -> AsyncResult.ofResult (f x1 x2 x3))
    let hoist4 f x1 x2 x3 x4 = IO (fun _ -> AsyncResult.ofResult (f x1 x2 x3 x4))

    let hoistOk f x1 = hoist (f>>Ok) x1
    let hoistOk2 f x1 x2 = IO (fun _ -> AsyncResult.ofOk (f x1 x2))
    let hoistOk3 f x1 x2 x3 = IO (fun _ -> AsyncResult.ofOk (f x1 x2 x3))
    let hoistOk4 f x1 x2 x3 x4 = IO (fun _ -> AsyncResult.ofOk (f x1 x2 x3 x4))

    let flatten m = bind id m
    let flattenResult m = bind (fun r -> ofResult r) m
    let flattenAsync m = bind (fun r -> ofAsync r) m
    let flattenAsyncResult m = bind (fun r -> ofAsyncResult r) m

    let extractResult m =
        IO (fun r -> let x = unsafeRun r m in AsyncResult.bimap Ok Error x |> AsyncResult.ofAsync)

    let extractResultSafe error m =
        IO (fun r -> let x = run error r m in AsyncResult.bimap Ok Error x |> AsyncResult.ofAsync)

    let tapError error f m =
        fun r -> async {
            match! run error r m with
            | Ok v -> return (Ok v)
            | Error e ->
                let! _ = unsafeRun e f
                return (Error e)
        } |> IO

    let tap f m =
        fun r -> async {
            match! unsafeRun r m with
            | Ok v ->
                let! _ = unsafeRun v f
                return (Ok v)
            | Error e ->
                return (Error e)
        } |> IO

    let readAllBytes (fileLocation : LocationContext) =
        let read path = hoistOk System.IO.File.ReadAllBytes path
// TODO convert to dotnet 3
//                System.IO.File.ReadAllBytesAsync
//                |> Async.AwaitTask
//                |> Async.map Ok

        LocationContext.path fileLocation
        |> ofResult
        |> bind read

    let writeAllBytes fileLocation (data:byte[]) =
        let write path =
            fun _ ->
                System.IO.File.WriteAllBytes(path, data)
                |> AsyncResult.ofOk
            |> IO
        LocationContext.path fileLocation
        |> ofResult
        |> bind write

    let createDirectory dirLocation =
        let create dir = hoistOk System.IO.Directory.CreateDirectory dir
        LocationContext.path dirLocation
        |> ofResult
        |> bind create

    // diagnostic information
//    let private readAllText fileLocation : IO<unit, string, GenericError> =
//        fun _ ->
//            asyncResult {
//                let! path = LocationContext.path fileLocation |> AsyncResult.ofResult
//                return System.IO.File.ReadAllTextAsync path |> Async.AwaitTask
//            }
//        |> IO
//        |> IO.flattenAsync

    let readAllText fileLocation : IO<unit, string, GenericError> =
        let readAllText path =
            try
                System.IO.File.ReadAllText path |> Ok
            with
            | ex -> Error (GenericError.Exception ex)

        let read path = hoistOk readAllText path |> flattenResult
        LocationContext.path fileLocation
        |> ofResult
        |> bind read

    let writeAllText fileLocation (data:string) =
        let write path =
            fun _ ->
                let dstDir = System.IO.Path.GetDirectoryName(path:string)
                System.IO.Directory.CreateDirectory(dstDir) |> ignore
                System.IO.File.WriteAllText(path, data)
                |> AsyncResult.ofOk
            |> IO
        LocationContext.path fileLocation
        |> ofResult
        |> bind write

    let copyFile (src : LocationContext) (dst : LocationContext) =
        let copy src dst =
            fun _ ->
                try
                    let dstDir = System.IO.Path.GetDirectoryName(dst:string)
                    System.IO.Directory.CreateDirectory(dstDir) |> ignore
                    System.IO.File.Copy(src, dst, true) |> Ok
                with
                | e -> (LoggableError (sprintf "Could not copy %s to %s (%s)" src dst e.Message)) |> Error
                |> AsyncResult.ofResult
            |> IO

        result {
            let! src = LocationContext.path src
            let! dst = LocationContext.path dst
            return src, dst
        }
        |> ofResult
        |> bind (uncurry copy)

    let directoryExists (path: LocationContext) =
        LocationContext.path path
        |> ofResult
        |> bind (hoistOk System.IO.Directory.Exists)

    let deleteDirectoryIfExists (path: LocationContext) =
        LocationContext.path path
        |> ofResult
        |> bind (fun dir ->
            fun _ ->
                try
                    if System.IO.Directory.Exists dir
                    then System.IO.Directory.Delete(dir, true) |> Ok
                    else Ok ()
                with
                | e -> (LoggableError (sprintf "Could not delete %s (%s)" dir e.Message)) |> Error
                |> AsyncResult.ofResult
            |> IO )

    let mapResult f = map f >> flattenResult

    let sequenceOption  =
        function
        | Some x -> x |> map Some
        | None -> ret None


//[<AutoOpen>]
module IoOperators =
    let ( >>= ) m f = IO.bind f m
    let ( =<< ) f m = IO.bind f m
    let ( <&> ) m f = IO.map f m
    let ( <!> ) f m = IO.map f m
    let ( <*> ) f m = IO.apply f m

type IOBuilder () =
    member _.Return x = IO.ret x
    member _.ReturnFrom x : IO<_, _, _> = x
    member _.Delay (f : unit -> IO<_, _, _>) = f
    member _.Combine (io, f) = IO.bind f io
    member _.Run (f : unit -> IO<_, _, _>) =
        fun ctx ->
            IO.unsafeRun ctx (f())
        |> IO
    member _.Bind (m, f) = IO.bind f m
    member _.Zero () = IO.empty

    member this.TryWith(f, handler) =
        fun ctx ->
            try
                IO.unsafeRun ctx (this.ReturnFrom(f()))
            with
            e -> IO.unsafeRun ctx (handler(e))
        |> IO

    member this.TryFinally(f, compensation) =
        fun ctx ->
            try
                IO.unsafeRun ctx (this.ReturnFrom(f()))
            finally
                compensation()
        |> IO

    member this.Using(disposable:#System.IDisposable, body) =
        let body' = fun () -> body disposable
        this.TryFinally(body', fun () ->
            match disposable with
            | null -> ()
            | disp -> disp.Dispose())

[<AutoOpen>]
module IoBuilder =
    let io = IOBuilder()
