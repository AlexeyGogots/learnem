namespace global

open System

[<RequireQualifiedAccess>]
module List =

    /// List applicative, works as the Cartesian product of the two lists
    let apply (fList: ('a -> 'b) list) (xList: 'a list)  =
        [ for f in fList do
          for x in xList do
              yield f x ]

    let private (<!>) = List.map
    let private (<*>) = apply

    /// Performs a rotation of elements to the left when count is positive and to the
    /// right when count is negative.
    let rotate count xs =
        let len = List.length xs
        let count = if xs.IsEmpty then 0 else count % len
        let calculateRotatedIndex i = ((i - count) + len) % len

        List.permute calculateRotatedIndex xs

    /// <summary>Returns a new list with the first element of the original list matching the predicate replaced</summary>
    /// <param name="predicate">The function to test which element to replace.</param>
    /// <param name="element">The new value to replace the original with.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The new list with the element replaced, paired with the old element or None.</returns>
    let replace predicate projection list =
        result {
            let! index =
                list
                |> List.tryFindIndex predicate
                |> Result.ofOption ""

            return! list
            |> List.splitAt index
            |> function
                | prelude, oldValue::tail -> Ok ((prelude@(projection oldValue)::tail), oldValue)
                | _ -> Error ""
        }
        |> Result.toOption

    /// <summary>Returns a new list with element at the given index replaced</summary>
    /// <param name="idx">The index of the element to replace.</param>
    /// <param name="element">The new value to replace the original with.</param>
    /// <param name="list">The input list.</param>
    /// <returns>The new list with the element replaced, paired with the old element or None.</returns>
    let replaceAt idx newValue list =
        result {
            return! list
            |> List.splitAt idx
            |> function
                | prelude, oldValue::tail -> Ok (prelude@newValue::tail, oldValue)
                | _ -> Error ""
        }
        |> Result.toOption

    /// Splits the list on the first element matching the given predicate into two lists.
    /// The first list contains all elements before the matching element, the second list
    /// contains the matching element followed by the list's tail.
    let splitAtFirst predicate list =
        list
        |> List.tryFindIndex predicate
        |> Option.map (fun index -> List.splitAt index list)
        |> Option.defaultValue (list, [])

    /// Returns the list of duplicates in the given list according to
    /// generic hash and equality comparison of the given projection of
    /// the entries
    let duplicatesBy projection list =
        let hasMultiple xs =
            1 < List.length xs

        list
        |> List.groupBy projection
        |> List.filter (snd >> hasMultiple)
        |> List.collect snd
        |> List.ofSeq

    /// Returns a triple of added, removed and changed entries in the
    /// two lists:
    /// - Added: the entries whose key is not found in `oldXs` and is found in `newXs`
    /// - Removed: the entries whose key is found in 'oldXs' and not in `newXs`
    /// - Changed: the entries whose key is found in both lists but have different value
    ///
    /// TODO: A better implementation might be:
    /// ```
    /// type DiffResult<'a> =
    ///     | Left of 'a
    ///     | Right of 'a
    ///     | Unequal of 'a * 'a
    /// symmetricDiff fKey fValue leftXs rightXs : DiffResult<'a> list
    /// ```
    let diffBy fKey fValue oldXs newXs =
        let normalize = List.sortBy fKey >> List.distinctBy fKey
        let oldXs = normalize oldXs
        let newXs = normalize newXs
        let hasKey xs x = let k = fKey x in List.exists (fun x -> k = fKey x) xs

        let newCommon, added = newXs |> List.partition (hasKey oldXs)
        let oldCommon, removed = oldXs |> List.partition (hasKey newXs)

        assert (newCommon.Length = oldCommon.Length)

        let changed =
            List.zip oldCommon newCommon
            |> List.filter (fun (o, n) -> fValue o <> fValue n)
            |> List.map snd

        added, removed, changed

    let tryExactlyOne = function
        | [a] -> Some a
        | _ -> None

    /// Returns a new list with the one and only element matching the
    /// predicate mapped.
    let tryMapExactlyOne select map xs =
        let f x (mappedCount, xs') =
            if select x
            then (mappedCount + 1, map x::xs')
            else (mappedCount, x::xs')

        List.foldBack f xs (0, [])
        |> function
            | (1, xs)  -> Some xs
            | _  -> None

    let cons x xs = x :: xs

    let rec tryTake n =
        function
        | x :: xs when n > 0 -> x :: tryTake (n - 1) xs
        | _ -> []
