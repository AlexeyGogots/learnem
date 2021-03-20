namespace global

open System
open System.Diagnostics

[<NoComparison>]
type GenericError =
| LoggableError of string
| Exception of Exception

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GenericError =

    let errorMessage = function
        | LoggableError err -> err
        | Exception ex -> ex.Message
    /// Writes the description prefix and error to the trace listeners before returning
    /// the error back to the caller

    let toException = function
        | Exception ex -> Some ex
        | _ -> None

    let traced (description:string) e =
        System.Diagnostics.Trace.TraceWarning("{0} [{1}]", description.Trim(), errorMessage e)
        e
    let log = errorMessage >> Trace.TraceWarning

