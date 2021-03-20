namespace Learnem
open System
open System.Text.RegularExpressions

[<AutoOpen>]
module Implementation =

    let createFlashcard (a, b) =
        let rec f (m:Match) : string list =
            if m.Success
            then m.Groups.["1"].Value :: (f (m.NextMatch()))
            else []

        let ``aaa(e)`` = Regex.Match (b, @"(?<1>[\w\s]+)\((?<2>\w)\)")
        let ``aaa (aaa)`` = Regex.Match (b, @"(?<1>[\w\s]+)\((?<2>[\w\s]+)\)")
        let ``aaa[,/] aaa`` = Regex.Match (b, @"(?<1>[\w\s]+)[,/]?")
        match ``aaa(e)``.Success, ``aaa (aaa)``.Success, ``aaa[,/] aaa``.Success with
        | true, _, _ -> [``aaa(e)``.Groups.["1"].Value; ``aaa(e)``.Groups.["1"].Value + ``aaa(e)``.Groups.["2"].Value]
        | _, true, _ -> [``aaa (aaa)``.Groups.["1"].Value; ``aaa (aaa)``.Groups.["2"].Value]
        | _, _, true -> f (``aaa[,/] aaa``)
        | _ -> [b]
        |> List.map (fun x -> x.Trim())
        |> List.map Definition
        |> fun x -> (Term a, x)
        |> Flashcard

//    let prepareWorkSet (flashcards : Flashcard list) (journal : Journal) : WorkSet =
