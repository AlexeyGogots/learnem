module Learnem.Tests

open System
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("negative", "negatief (negatieve)", "negatief, negatieve")>]
[<InlineData("necessary", "nodig (e)", "nodig, nodige")>]
[<InlineData("nature", "de natuur", "de natuur")>]
[<InlineData("thin", "dun/dunne", "dun, dunne")>]
[<InlineData("to watch, to look at", "kijken (naar), bekijken", "kijken (naar), bekijken")>]
[<InlineData("test", "test1, test2, test3", "test1, test2, test3")>]
let ``Can create a term`` (a:string, b:string, definition:string) =

    let card = createFlashcard (a, b)
    printfn "%A" card
    let definitions =
        definition.Split(',')
        |> Array.map (fun x -> x.Trim())
        |> Array.map Definition
        |> Array.toList


    <@ card = Flashcard (Term a, definitions)  @>
