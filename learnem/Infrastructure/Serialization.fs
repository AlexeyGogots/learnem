namespace Learnem.Serialization

open Newtonsoft.Json
open System.Reflection
open System.Runtime.CompilerServices

//open System.Collections.Concurrent

/// Json (de)serializer that works in a strict way:
/// - F# friendly API
/// - Formats F# types in a compact way (thanks to FSharpLu.Json):
///   - Single case discriminated unions
///   - Discriminated unions without fields (i.e. C# enums)
///   - Discriminated unions with fields
///   - Enums serialized as strings
/// - Formats TranslatableStrings and MaybeTranslatableStrings in a readable way
/// - All (non-optional) fields are required
///
/// The implementation is based on existing libraries (FShardLu and NewtonSoft).
/// We just `improve` their existing stuff to match our needs (e.g. to add support
/// for translatable strings).
module Strict =

    module private Internals =
        let toResult choice =
            match choice with
            | Choice.Choice1Of2 a -> Ok a
            | Choice.Choice2Of2 b -> Error b

    /// A contract resolver that requires presence of all properties
    /// that are not of type option<_>, this is heavily based on the
    /// revolver with the same name used in FSharpLu.Json, the only
    /// difference being the base resolver used. See
    ///    https://github.com/microsoft/fsharplu/issues/92
    /// for more information and to see if we can start using
    ///    FSharpLu.Json.Compact.Strict.RequireNonOptionalPropertiesContractResolver
    /// again
    type RequireNonOptionalPropertiesContractResolver () =
        inherit Newtonsoft.Json.Serialization.DefaultContractResolver()

        override _.CreateProperty (``member``, memberSerialization) =
            let property = base.CreateProperty(``member``, memberSerialization)

            let isRequired =
                not
                    (property.PropertyType.GetTypeInfo().IsGenericType
                     && property.PropertyType.GetGenericTypeDefinition() = typedefof<option<_>>)

            if isRequired then
                property.Required <- Required.Always
                property.NullValueHandling <- System.Nullable NullValueHandling.Ignore

            property


    type CompactStrictSettings =
        static member formatting = Newtonsoft.Json.Formatting.Indented

        static member settings =
            Newtonsoft.Json.JsonSerializerSettings
                (ContractResolver = RequireNonOptionalPropertiesContractResolver(),
                 Converters =
                     [| SingleCaseUnionConverter()
                        Newtonsoft.Json.Converters.StringEnumConverter()
                        Microsoft.FSharpLu.Json.CompactUnionJsonConverter(true) |])

    type private S = Microsoft.FSharpLu.Json.With<CompactStrictSettings>

    /// Serialize an object of type ^T to a json-formatted string
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline serialize< ^T> x = S.serialize x

    /// Try to deserialize a json-formatted string to an object of type ^T
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline deserialize< ^T> = S.tryDeserialize< ^T> >> Internals.toResult

    /// Deserialize a json-formatted stream to an object of type ^T
    [<MethodImplAttribute(MethodImplOptions.NoInlining)>]
    let inline deserializeStream< ^T> = S.tryDeserializeStream< ^T> >> Internals.toResult
