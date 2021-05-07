namespace Learnem.Serialization

open Microsoft.FSharp.Reflection
open Newtonsoft.Json

/// NewtonSoft.Json JSON converter to clean JSON serialization of simple types:
/// E.g. the type
/// ```
///    type Id = Id of string
/// ```
/// will be serialized as a string instead of a discriminated union with one case.
type SingleCaseUnionConverter () =
    inherit Newtonsoft.Json.JsonConverter()

    let toUnionCases (objectType:System.Type) =
        // It seems that both option and list are implemented using discriminated unions,
        // so tell json.net to ignore them and use different serializer
        let isActualUnion =
               FSharpType.IsUnion objectType
            && not (FSharpType.IsRecord objectType)
            && not (objectType.IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<List<_>>)
            && not (objectType.IsGenericType && objectType.GetGenericTypeDefinition() = typedefof<Option<_>>);

        if isActualUnion
        then objectType |> FSharpType.GetUnionCases |> Some
        else None

    let isSingleCaseUnion (cases:UnionCaseInfo[]) =
        cases.Length = 1 && cases.[0].GetFields().Length = 1

    let toSingleCaseUnion cases =
        if isSingleCaseUnion cases
        then Some cases.[0]
        else None

    let writeValue (value:obj, serializer:JsonSerializer, writer:JsonWriter) =
        if value.GetType().IsPrimitive
        then writer.WriteValue(value)
        else serializer.Serialize(writer, value)

    override _.CanConvert objectType =
        objectType
        |> toUnionCases
        |> Option.bind toSingleCaseUnion
        |> Option.isSome

    override _.ReadJson(reader:JsonReader, objectType:System.Type, existingValue:obj, serializer:JsonSerializer) =
        let optionalCase =
            objectType
            |> toUnionCases
            |> Option.bind toSingleCaseUnion

        match optionalCase with
        | None -> (raise (JsonSerializationException("Unexpected JSON token while reading in SingleCaseUnionConverter")))
        | Some case ->
            let caseValue = serializer.Deserialize(reader, case.GetFields().[0].PropertyType);
            let unionValue = FSharpValue.MakeUnion(case, [|caseValue|]);
            unionValue

    override _.WriteJson(writer:JsonWriter, value:obj, serializer:JsonSerializer) =
        let unionCases = FSharpType.GetUnionCases(value.GetType())
        let unionType = value.GetType()
        let (_, fields) = FSharpValue.GetUnionFields(value, unionType)

        if not (isSingleCaseUnion unionCases)
        then failwith "This JSON converter only allows single case unions"

        writeValue(fields.[0], serializer, writer)
