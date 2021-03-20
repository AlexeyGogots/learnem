namespace Learnem

open System

type Flashcard = Flashcard of Term * Definition list
and Term = Term of string
and Definition = Definition of string

type Journal = Journal of (Flashcard * Timestamp * bool list) list
and Status =
    | Started
    | Learned
    | Mastered
and Timestamp = Timestamp of DateTimeOffset

type WorkSet = WorkSet of Flashcard list
