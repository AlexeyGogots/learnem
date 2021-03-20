namespace global

open System


[<NoComparison>]
[<NoEquality>]
type LocationContext =
    private
        | Absolute of (unit -> Result<Uri, GenericError>)
        | Relative of (unit -> Result<Uri, GenericError>)
        | Relatives of (unit -> Result<Uri, GenericError>) list

/// <summary><code>a /. b</code> Concatenates two LocationContext</summary>
/// <returns>
/// If <code>a</code> is an absolute Uri returns a concatenation result with <code>b</code>
/// If <code>a</code> is a relative Uri returns <code>b</code>
/// </returns>
/// <remarks>
///     A directory must contain a tailing /
/// </remarks>
/// <exmaple>
/// absolute "c:/foo/bar/" /. relative "buzz" returns c:/foo/bar/buzz
/// absolute "c:/foo/bar" /. relative "buzz" returns c:/foo/buzz
/// absolute "c:/foo/bar" /. absolute "c:/buzz" returns c:/buzz
/// relative "foo" /. relative "buzz" return buzz
/// </example>
[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LocationContext =
    let private (!) (a:string) = a.Replace("\\", "/")

    let private dir (a:string) = (!a).TrimEnd('/') + "/"

    let private toUri (kind:UriKind) (a:string) =
        match  Uri.TryCreate(!a, kind) with
        | true, uri -> Ok uri
        | false, _ -> sprintf "Failed turn [%s] into a Uri(%O)" (!a) kind |> LoggableError |> Error

    let private run = function
        | Absolute f | Relative f -> f ()
        | Relatives _ -> sprintf "Location must be absolute path" |> LoggableError |> Error

    // bind' because it's not a classical bind where f should return LocationContext
    let private bind' (f:Uri -> Result<Uri, GenericError>) m =
        match m with
        | Absolute _ -> Absolute (fun _ -> run m |> Result.bind f )
        | _ -> Relative (fun _ -> run m |> Result.bind f )

    let path m =
        run m
        |> Result.map (fun uri -> if uri.IsAbsoluteUri then uri.LocalPath else uri.OriginalString)

    let private error (uri:Uri) = sprintf "Not absolute URI : %s" uri.OriginalString |> LoggableError |> Error

    let uri m =
        run m
        |> Result.bind (fun uri -> if uri.IsAbsoluteUri then Ok uri.AbsoluteUri else error uri)

    let absolutePath m =
        run m
        |> Result.bind (fun uri -> if uri.IsAbsoluteUri then Ok uri.LocalPath else error uri)

    [<CompiledName("Absolute")>]
    let absolute a = Absolute (fun _ -> toUri UriKind.Absolute a)

    let absoluteDir (a:string) = dir a |> absolute

    [<CompiledName("Relative")>]
    let relative a = Relative (fun _ ->
        match absolute a |> run with
        | Ok _ -> a |> sprintf "The given uri [%s] is not relative" |> GenericError.LoggableError |> Error
        | Error _ -> toUri UriKind.Relative a)

    [<CompiledName("RelativeDir")>]
    let relativeDir (a:string) = dir a |> relative

    let private relativeOrAbsolute (uri : Uri) =
        match uri.IsAbsoluteUri with
        | true -> Absolute (fun _ -> Ok uri)
        | _ -> Relative (fun _ -> Ok uri)

    let private absoluteError e =
        Absolute (fun _ -> Error e)

    [<CompiledName("MakeRelativeTo")>]
    let makeRelativeTo baseLocation location =
        result {
            let! baseUri = baseLocation |> run
            let! locUri = location |> run

            try
                // .net standard 2.0 does not have a Path relative to function so we use the deprecated Uri instead
                // https://weblog.west-wind.com/posts/2010/Dec/20/Finding-a-Relative-Path-in-NET
                return baseUri.MakeRelativeUri(locUri)
            with
                _ -> return! sprintf "Cannot make %s relative to %s" baseUri.OriginalString locUri.OriginalString |> LoggableError |> Error
        }
        |> Result.bimap relativeOrAbsolute absoluteError

    let (/.) (a:LocationContext) (b:LocationContext) =
        let error (ex:Exception) (left:Uri) (right:Uri) =
            sprintf "Invalid location composition %s /. %s\r\n %s" left.OriginalString right.OriginalString ex.Message
            |> GenericError.LoggableError
            |> Error
        let g (left:Uri) (right:Uri) = try Uri (left, right) |> Ok with ex -> error ex left right
        let f b (left:Uri) = run b |> Result.bind (fun right -> g left right )

        match a, b with
        | Absolute _, Relative _ -> bind' (f b) a
        | Absolute _, Relatives rs -> List.foldBack (fun rel lc -> bind' (f (Relative rel)) lc) rs a
        | Relative r1, Relative r2 -> Relatives [r2; r1]
        | Relatives rs1, Relatives rs2 -> Relatives (rs2 @ rs1)
        | Relative r, Relatives rs -> Relatives (rs @ [r])
        | Relatives rs, Relative r -> Relatives (r::rs)
        | _, Absolute _ -> bind' (f b) a // will be an error

    let sequence = function
        | Ok v -> v
        | Error err -> Absolute (konst (Error err))

