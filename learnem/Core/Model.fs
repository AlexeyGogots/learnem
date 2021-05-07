namespace Learnem

open System
open System.Text.RegularExpressions

type Flashcard = Flashcard of Term * Definition list

and Flashcards = Flashcards of Map<Term, Definition list>

and Term = Term of string

and Definition = private Definition of string

type Journal = Journal of Map<Term, Timestamp * Answers>

and Answers = Answers of bool list

and AnswerResult =
    | Incorrect
    | Correct
    | AlmostCorrect
    | MissedArticle

and Timestamp = Timestamp of DateTimeOffset

type WorkSet = WorkSet of Flashcard list

module Definition =
    let make (str : string) =
        str.Trim()
        |> Seq.toList
        |> List.foldBack (fun x xs ->
            match xs with
            | [] -> [ x ]
            | h :: _ when h = ' ' && x = ' ' -> xs
            | _ -> x :: xs) []
        |> (List.toArray >> String)
        |> Definition

module Flashcard =
    let private trimArticle s = Regex.Replace(s, "^het\s|^de\s", "")

    let hint (Flashcard (_, ds)) =
        ds
        |> List.map (fun (Definition d) ->
            let d = trimArticle d

            d.Trim()
            |> Seq.mapi (fun i x -> if Char.IsSeparator x || i % 3 = 0 then x else '*')
            |> Seq.toList
            |> (List.toArray >> String))


    let term (Flashcard (x, _)) = x

    let check (Flashcard (Term _, xs)) (Definition a) =
        let correct =
            xs
            |> List.exists (fun (Definition x) -> String.Equals(x, a, StringComparison.InvariantCultureIgnoreCase))

        let almostCorrect =
            let compare (x : string) =
                let compare xys =
                    xys
                    |> Seq.fold (fun (l, c) (x, y) ->
                        if c && String.Equals(string x, string y, StringComparison.InvariantCultureIgnoreCase)
                        then l + 1, c
                        else l, false)
                        (0, true)
                    |> fst

                let forward = Seq.zip x a |> compare
                let backward = Seq.zip (Seq.rev x) (Seq.rev a) |> compare

                forward + backward <= x.Length + 2
                && forward + backward >= Math.Max(x.Length, a.Length) - 2
                && Math.Abs(a.Length - x.Length) <= 1


            xs |> List.exists (fun (Definition x) -> compare x)

        let missedArticle =
            xs
            |> List.exists (fun (Definition x) ->
                String.Equals(trimArticle x, a, StringComparison.InvariantCultureIgnoreCase))

        match correct, missedArticle, almostCorrect with
        | true, _, _ -> Correct
        | false, true, _ -> MissedArticle
        | false, _, true -> AlmostCorrect
        | _ -> Incorrect


    let answers (Flashcard (Term _, ds)) = ds |> List.map (fun (Definition x) -> x)

module Answers =
    let add x (Answers xs) = x :: xs |> List.tryTake 10 |> Answers

module Journal =
    let empty = Journal Map.empty

    let record (Journal journal) term =
        journal
        |> Map.tryFind (term)
        |> Option.map snd
        |> Option.defaultValue (Answers [])

    let update (Journal journal) term timestamp answers = Map.add term (timestamp, answers) journal |> Journal

module Flashcards =
    let find (Flashcards xs) term =
        xs
        |> Map.tryFind term
        |> Option.map (fun definitions -> Flashcard(term, definitions))

    let count (Flashcards xs) = Map.count xs

    let exclude (Journal journal) (Flashcards cards) =
        journal
        |> Map.fold (fun cards term _ -> Map.remove term cards) cards
        |> Flashcards

    let toList (Flashcards xs) = xs |> Map.toList |> List.map Flashcard
    let toArray (Flashcards xs) = xs |> Map.toArray |> Array.map Flashcard

module WorkSet =
    let ask (WorkSet workset) : Flashcard option * WorkSet =
        match workset with
        | x :: xs -> Some x, WorkSet xs
        | _ -> None, WorkSet []
