namespace Learnem

open System
open System.Text.RegularExpressions

[<AutoOpen>]
module Implementation =

    let createFlashcard (a, b) =

        let rec parse b =
            let ``aaa(e)`` = Regex.Match(b, @"(?<1>[\w\s]+)\((?<2>e)\)")
            let ``aaa (aaa)`` = Regex.Match(b, @"(?<1>[\w\s]+)\((?<2>[\w\s]{2,})\)")
            let ``aaa[,/] aaa`` = Regex.Match(b, @"(?<1>.+)[,/](?<2>.+)")

            match ``aaa(e)``.Success, ``aaa (aaa)``.Success, ``aaa[,/] aaa``.Success with
            | _, _, true ->
                let x = ``aaa[,/] aaa``.Groups.["1"].Value.Trim() |> parse
                let y = ``aaa[,/] aaa``.Groups.["2"].Value.Trim() |> parse
                x @ y
            | true, _, _ ->
                [ ``aaa(e)``.Groups.["1"].Value
                  ``aaa(e)``.Groups.["1"].Value.Trim()
                  + ``aaa(e)``.Groups.["2"].Value.Trim() ]
            | _, true, _ ->
                let x = ``aaa (aaa)``.Groups.["1"].Value.Trim()
                let y = ``aaa (aaa)``.Groups.["2"].Value.Trim()

                let f =
                    Seq.fold (fun (count, stop) (x, y) ->
                        if x = y && (not stop) then (count + 1, stop) else (count, true)) (0, false)

                let root = Seq.zip x y |> f |> fst
                if root > 2 then [ x; y ] else [ x; sprintf "%s %s" x y ]
            | _ -> [ b ]

        parse b
        |> List.map Definition.make
        |> fun x -> (Term a, x) |> Flashcard

    let tryFind flashcards term = flashcards |> List.tryFind (Flashcard.term >> (=) term)

    let coolOffPeriod period (x : DateTimeOffset) (y : DateTimeOffset) = x - y >= period

    let getMastered dt (flashcards : Flashcards) (Journal xs) : Flashcard list =
        let find = Flashcards.find flashcards
        let coolOffPeriod = coolOffPeriod (TimeSpan.FromDays 11.) dt

        xs
        |> Map.filter (fun _ (_, Answers answers) -> answers.Length >= 10)
        |> Map.filter (fun _ (Timestamp dt, Answers answers) -> coolOffPeriod dt && answers |> List.take 10 |> List.forall id)
        |> Map.toList
        |> List.sortBy (fun (_, (Timestamp time, _)) -> time)
        |> List.choose (fun (term, _) -> find term)
        |> List.tryTake 1


    let getLearned dt (flashcards : Flashcards) (Journal xs) : Flashcard list =
        let find = Flashcards.find flashcards
        let coolOffPeriod = coolOffPeriod (TimeSpan.FromDays 5.) dt

        let isLearned dt answers =
            coolOffPeriod dt
            && answers |> List.take 5 |> List.forall id
            && (answers.Length = 5 || answers |> List.forall id |> not)

        xs
        |> Map.filter (fun _ (_, Answers answers) -> answers.Length >= 5 && answers.Length < 10)
        |> Map.filter (fun _ (Timestamp dt, Answers answers) -> isLearned dt answers)
        |> Map.toList
        |> List.map (fun (term, (time, Answers answers)) -> term, (answers, time))
        |> List.sortBy snd
        |> List.choose (fun (term, _) -> find term)
        |> List.tryTake 3

    let getStarted dt (flashcards : Flashcards) (Journal xs) : Flashcard list =
        let find = Flashcards.find flashcards
        let coolOffPeriod = coolOffPeriod (TimeSpan.FromDays 1.) dt

        let isStarted dt answers = coolOffPeriod dt || answers |> List.head |> not

        xs
        |> Map.filter (fun _ (_, Answers answers) -> answers.Length < 5 || answers |> List.tryTake 5 |> List.forall id |> not)
        |> Map.filter (fun _ (Timestamp dt, Answers answers) -> isStarted dt answers)
        |> Map.toList
        |> List.map (fun (term, (time, Answers answers)) -> term, (answers, time))
        |> List.sortBy snd
        |> List.choose (fun (term, _) -> find term)

    let shuffle (rnd : Random) (cards : _ list) =
        cards
        |> List.map (fun card -> rnd.Next(1000), card)
        |> List.sortBy fst
        |> List.map snd

    let prepareWorkSet count (cards : Flashcards) (journal : Journal) : IO<_, WorkSet, GenericError> =
        io {
            let now = DateTimeOffset.Now

            let familiar =
                (getMastered now cards journal )
                @ (getLearned now cards journal)
                @ (getStarted now cards journal)

            let count' = Math.Max(count - List.length familiar, 0)

            let cards = cards |> Flashcards.exclude journal |> Flashcards.toArray
            let totalCount = Array.length cards - count'

            let rnd = Random(now.Millisecond)

            return
                [ 1 .. count' * 10 ]
                |> List.map (fun _ -> rnd.Next(totalCount - 1))
                |> List.distinct
                |> List.tryTake count'
                |> List.map (fun index -> cards.[index])
                |> List.append familiar
                |> List.tryTake count
                |> shuffle rnd
                |> WorkSet
        }

    let rec ask card =
        io {
            let (Term term) = Flashcard.term card
            printf "%s: " term

            match Console.ReadLine() with
            | "" ->
                Flashcard.hint card |> List.iter (printfn "• %s")
                printfn ""
                return! ask card
            | "?" ->
                Console.ForegroundColor <- ConsoleColor.Yellow
                Flashcard.answers card |> List.iter (printfn "• %s")
                Console.ResetColor()
                printfn ""

                let! _ = ask card
                return false
            | answer ->
                match answer |> Definition |> Flashcard.check card with
                | Correct ->
                    Console.CursorTop <- Console.CursorTop - 1
                    printf "\r%s: " term
                    Console.ForegroundColor <- ConsoleColor.Green
                    printfn "%s" answer
                    Console.ForegroundColor <- ConsoleColor.DarkGreen

                    List.foldBack (fun x xs -> if x = answer then xs else x :: xs) (Flashcard.answers card) []
                    |> List.iter (printfn "• %s")

                    Console.ResetColor()
                    printfn ""
                    return true

                | MissedArticle ->
                    Console.CursorTop <- Console.CursorTop - 1
                    printf "\r%s: " term
                    Console.ForegroundColor <- ConsoleColor.Yellow
                    printfn "? %s" answer

                    Console.ResetColor()
                    printfn ""
                    return! ask card

                | AlmostCorrect ->
                    Console.CursorTop <- Console.CursorTop - 1
                    printf "\r%s: " term
                    Console.ForegroundColor <- ConsoleColor.Yellow
                    printfn "%s" answer

                    Console.ResetColor()
                    printfn ""
                    return! ask card

                | Incorrect ->
                    Console.CursorTop <- Console.CursorTop - 1
                    printf "\r%s: " term
                    Console.ForegroundColor <- ConsoleColor.Red
                    printfn "%s" answer
                    Console.ForegroundColor <- ConsoleColor.Yellow
                    Flashcard.answers card |> List.iter (printfn "• %s")

                    Console.ResetColor()
                    printfn ""
                    let! _ = ask card
                    return false
        }

    let session journal (WorkSet workset) =
        List.foldBack (fun card journal ->
        io {
            let! journal = journal
            let term = Flashcard.term card
            let! answer = ask card
            let answers = Answers.add answer <| Journal.record journal term
            return Journal.update journal term (Timestamp DateTimeOffset.Now) answers
        }) workset (IO.ret journal)
