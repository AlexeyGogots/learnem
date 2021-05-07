module Learnem.Tests

open System
open Xunit
open Swensen.Unquote

let testFlashcards =
    [ "cool-off"
      "to-take4"
      "to-take10"
      "learned-cool-off"
      "learned-to-take"
      "mastered-cool-off"
      "mastered-to-skip"
      "mastered-to-take" ]
    |> List.map (fun x -> (Term x, [ Definition.make x ]))
    |> Map.ofList
    |> Flashcards

let testJournal =
    let now = DateTimeOffset.Now

    [ "cool-off", (now, [ true; true; false; true ])
      "to-take10",
      (now,
       [ false
         true
         true
         true
         true
         true
         true
         true
         true
         true ])
      "learned-cool-off", (now, [ true; true; true; true; true ])
      "learned-to-take", (now - (TimeSpan.FromDays 6.), [ true; true; true; true; true ])
      "mastered-cool-off",
      (now,
       [ true
         true
         true
         true
         true
         true
         true
         true
         true
         true ])
      "mastered-to-skip",
      (now - (TimeSpan.FromDays 8.),
       [ true
         true
         true
         true
         true
         true
         true
         true
         true
         true ])
      "mastered-to-take",
      (now - (TimeSpan.FromDays 12.),
       [ true
         true
         true
         true
         true
         true
         true
         true
         true
         true ]) ]
    |> List.map (fun (term, (timestamp, answers)) -> Term term, (Timestamp timestamp, Answers answers))
    |> Map.ofList
    |> Journal


[<Theory>]
[<InlineData("mastered-to-take", 0.)>]
[<InlineData("", 12.)>]
let ``get mastered`` (expected, daysBack) =
    let x =
        Implementation.getMastered (DateTimeOffset.Now - TimeSpan.FromDays daysBack) testFlashcards testJournal
        |> List.map Flashcard.term

    let expected = if expected = "" then [] else [(Term expected)]
    test <@ x = expected @>

[<Theory>]
[<InlineData("learned-to-take", 0.)>]
[<InlineData("", 6.)>]
let ``get learned`` (expected, daysBack) =
    let x =
        Implementation.getLearned (DateTimeOffset.Now - TimeSpan.FromDays daysBack) testFlashcards testJournal
        |> List.map Flashcard.term

    let expected = if expected = "" then [] else [ (Term expected) ]
    test <@ x = expected @>

[<Theory>]
[<InlineData("to-take10", 0.)>]
[<InlineData("to-take10", 7.)>]
let ``get started`` (expected, hoursBack) =
    let x =
        Implementation.getStarted (DateTimeOffset.Now - TimeSpan.FromHours hoursBack) testFlashcards testJournal
        |> List.map Flashcard.term

    let expected = if expected = "" then [] else [ (Term expected) ]
    test <@ x = expected @>

[<Theory>]
[<InlineData(1, 100, true)>]
[<InlineData(1, 50, false)>]
let ``cool off`` (recorded, current, expected) =
    let recorded = DateTimeOffset.FromUnixTimeSeconds recorded
    let current = DateTimeOffset.FromUnixTimeSeconds current
    let x = coolOffPeriod (TimeSpan.FromSeconds 50.) current recorded

    test <@ x = expected @>

[<Theory>]
[<InlineData("negative", "negatief (negatieve)", "negatief, negatieve")>]
[<InlineData("necessary", "nodig (e)", "nodig, nodige")>]
[<InlineData("nature", "de natuur", "de natuur")>]
[<InlineData("thin", "dun/dunne", "dun, dunne")>]
[<InlineData("to watch, to look at", "kijken (naar), bekijken", "kijken, kijken naar, bekijken")>]
[<InlineData("test", "test1, test2, test3", "test1, test2, test3")>]
let ``Can create flashcard`` (a : string, b : string, definition : string) =

    let (Flashcard (Term term, definitions)) = createFlashcard (a, b)

    let expected =
        definition.Split(',')
        |> Array.map Definition.make
        |> Array.toList

    test <@ term = a @>
    test <@ definitions = expected @>

[<Theory>]
[<InlineData("negatief", "neagtief", true)>] // swapped letter
[<InlineData("negatief", "neagtife", false)>] // twice swapped letters
[<InlineData("negatief", "negatif", true)>] // missed letter
[<InlineData("negatief", "negati", false)>] // two missed letters
[<InlineData("negatief", "negattief", true)>] // extra letter
[<InlineData("negatief", "negattieff", false)>] // two extra letters
[<InlineData("praten", "spreken", false)>] // wrong answer
[<InlineData("het gevoel", "de gevoel", false)>] // wrong article
[<InlineData("de landbouw", "het landbouw", false)>] // wrong article
let ``almost correct`` (definition : string, answer : string, expected : bool) =

    let card = createFlashcard ("term", definition)
    let answer = Definition.make answer

    let result =
        match Flashcard.check card answer with
        | AlmostCorrect -> true
        | _ -> false

    test <@ result = expected @>

[<Theory>]
[<InlineData("het gevoel", "gevoel", true)>]
[<InlineData("het gevoel", "het gevoel", false)>]
let ``missed article`` (definition : string, answer : string, expected : bool) =

    let card = createFlashcard ("term", definition)
    let answer = Definition.make answer

    let result =
        match Flashcard.check card answer with
        | MissedArticle -> true
        | _ -> false

    test <@ result = expected @>

[<Theory>]
[<InlineData("het gevoel", "de gevoel", false)>]
[<InlineData("het gevoel", "het gevoel", true)>]
[<InlineData("het gevoel", "het geoel", false)>]
[<InlineData("het gevoel", "gevoel", false)>]
[<InlineData("het gevoel", "leoveg teh", false)>]
let ``correct`` (definition : string, answer : string, expected : bool) =

    let card = createFlashcard ("term", definition)
    let answer = Definition.make answer

    let result =
        match Flashcard.check card answer with
        | Correct -> true
        | _ -> false

    test <@ result = expected @>
