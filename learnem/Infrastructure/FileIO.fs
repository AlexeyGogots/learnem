module Learnem.FileIO

open System
open FSharp.Data
open Learnem.Serialization
open Newtonsoft.Json

module Dto =
    type DtoJournalRecord =
        { [<JsonProperty("Term")>]
          Term : string
          [<JsonProperty("Answers")>]
          Answers : bool list
          [<JsonProperty("Timestamp")>]
          Timestamp : DateTimeOffset }

    type DtoJournal =
        { [<JsonProperty("Records")>]
          Records : DtoJournalRecord list }

    let toDto (Journal xs) =
        xs
        |> Map.toList
        |> List.map (fun ((Term term), ((Timestamp time), (Answers answers))) ->
            { Term = term
              Answers = answers
              Timestamp = time })
        |> fun xs -> { DtoJournal.Records = xs }

    let fromDto (dto : DtoJournal) =
        dto.Records
        |> List.map (fun x -> (Term x.Term, (Timestamp x.Timestamp, Answers x.Answers)))
        |> Map.ofList
        |> Journal

let saveJournal (path : LocationContext) (journal : Journal) : IO<unit, unit, GenericError> =
    let dto = Dto.toDto journal
    let serialized = Strict.serialize dto
    IO.writeAllText path serialized

let loadJournal (path : LocationContext) : IO<unit, Journal, GenericError> =
    io {
        let! text = IO.readAllText path
        let! dto = Strict.deserialize text |> IO.ofResult |> IO.mapError LoggableError
        return Dto.fromDto dto
    }

let loadJournalOrDefault ``default`` (path : LocationContext) : IO<unit, Journal, GenericError> =
    loadJournal path
    |> IO.ifError (fun _ -> ``default``)

let loadFlashcards (filename : LocationContext) : IO<_, Flashcards, GenericError> =
    let flashcards words =
        let deduplicate (x, y) (m : Map<string, string>) =
            match Map.tryFind x m with
            | None -> Map.add x y m
            | Some ys -> Map.add x (sprintf "%s, %s" ys y) m

        List.foldBack (fun (x, y) m -> deduplicate (x, y) m) words Map.empty
        |> Map.fold (fun m x y ->
            let (Flashcard (term, definitions)) = createFlashcard (x, y) in Map.add term definitions m) Map.empty
        |> Flashcards

    io {
        let! filename = LocationContext.path filename |> IO.ofResult
        let (file, _, _) = CsvFile.Load filename, "", false
        let loaded = file.Rows |> Seq.map (fun row -> row.Columns.[0], row.Columns.[1]) |> Seq.toList |> flashcards
        return loaded
    }
