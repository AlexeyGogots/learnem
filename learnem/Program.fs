open System
open System.Globalization
open System.IO
open Learnem
open Nucleus
open FSharp.Data




let duplicates words =
    let duplicate (x:'a) (m:Map<'a, int>) =
        match Map.tryFind x m with
        | None -> Map.add x 1 m
        | Some count -> Map.add x (count+1) m

    List.foldBack (fun (x, y) m -> duplicate (x, y) m) words Map.empty
    |> Map.filter (fun _ y -> y > 1)
    |> Map.iter (fun x y -> printfn "%s, %i" (fst x) y)

let flashcards words =
    let duplicate (x, y) (m:Map<string, string>) =
        match Map.tryFind x m with
        | None -> Map.add x y m
        | Some ys -> Map.add x (sprintf "%s, %s" ys y)  m

    List.foldBack (fun (x, y) m -> duplicate (x, y) m) words Map.empty
    |> Map.toList
    |> List.map createFlashcard

let loadFlashcards (filename : string) : IO<_, Flashcard list, string> =
    io {
        let (file, _, _) = CsvFile.Load filename, "", false
        let loaded =
            file.Rows
            |> Seq.map (fun row -> row.Columns.[0], row.Columns.[1])
            |> Seq.toList
            |> flashcards
        return loaded
    }



[<EntryPoint>]
let main argv =


//    loadFlashcards @"C:\repo\learnem\learnem\data\worden.tsv"
//    |> IO.runSynchronously

    let x = createFlashcard("test", "test1, test2, test3")

    0 // return an integer exit code
