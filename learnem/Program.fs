open System
open System.Globalization
open System.IO
open Learnem
open FSharp.Data

let duplicates words =
    let duplicate (x : 'a) (m : Map<'a, int>) =
        match Map.tryFind x m with
        | None -> Map.add x 1 m
        | Some count -> Map.add x (count + 1) m

    List.foldBack (fun (x, y) m -> duplicate (x, y) m) words Map.empty
    |> Map.filter (fun _ y -> y > 1)
    |> Map.iter (fun x y -> printfn "%s, %i" (fst x) y)

let printStats (Journal journal, totalCount) =
    journal
    |> Map.fold (fun acc _ (_, (Answers answers)) ->
        match answers with
        | _ when answers.Length >= 10 && List.forall id answers ->
            {| Mastered = acc.Mastered + 1
               Learned = acc.Learned
               Started = acc.Started |}
        | _ when answers.Length >= 5
                 && answers |> List.take 5 |> List.forall id ->
            {| Mastered = acc.Mastered
               Learned = acc.Learned + 1
               Started = acc.Started |}
        | _ ->
            {| Mastered = acc.Mastered
               Learned = acc.Learned
               Started = acc.Started + 1 |})
           {| Mastered = 0
              Learned = 0
              Started = 0 |}

    |> fun stats ->
        printfn
            "%i(%i,%i,%i)/%i"
            (stats.Mastered + stats.Learned + stats.Started)
            stats.Started
            stats.Learned
            stats.Mastered
            totalCount

[<EntryPoint>]
let main args =

    io {
        Console.OutputEncoding <- System.Text.Encoding.Unicode

        let basePath =
            absoluteDir
            <| Path.GetDirectoryName
                (System
                    .Reflection
                    .Assembly
                    .GetExecutingAssembly()
                    .Location)

        let homePath =
            absoluteDir
            <| Environment.GetEnvironmentVariable("USERPROFILE")

        let cards =
            basePath
            /. LocationContext.relativeDir "Data"
            /. relative "worden.tsv"

        let cards = if args.Length > 0 then LocationContext.absolute args.[0] else cards

        let journalFile =
            homePath
            /. relativeDir ".learnem"
            /. relative "journal-worden.json"

        let saveJournal = FileIO.saveJournal journalFile
        let loadJournal = FileIO.loadJournalOrDefault Journal.empty journalFile
        let loadFlashcards = FileIO.loadFlashcards cards
        let! journal = loadJournal
        let! flashcards = loadFlashcards
        let! workset = prepareWorkSet 30 flashcards journal
        let! journal = session journal workset
        do! saveJournal journal
        return (journal, Flashcards.count flashcards)
    }
    |> IO.runSynchronously ()
    |> Result.bimap printStats (printfn "%A")

    Console.ReadKey() |> ignore
    0 // return an integer exit code
