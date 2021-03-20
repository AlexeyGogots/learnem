namespace global

module Tuple2 =

    let curry f x y = f (x, y)

    let uncurry f (x, y) = f x y

    let replicate x = x, x

    let replicateWith f1 f2 x = f1 x, f2 x

    let makeTuple x y = x, y

    let makeTupleRev y x = x, y

    let mapFst f (x, y) = f x, y

    let mapSnd f (x, y) = x, f y

    let bimap f1 f2 (x, y) = f1 x, f2 y

    let swap (x, y) = (y, x)

module Tuple3 =
    let fst (a, _, _) = a
    let snd (_, b, _) = b
    let trd (_, _, c) = c

    let dropFst (_, b, c) = (b, c)
    let dropSnd (a, _, c) = (a, c)
    let dropTrd (a, b, _) = (a, b)

    let mapFst f (a, b, c) = f a,   b,   c
    let mapSnd f (a, b, c) =   a, f b,   c
    let mapTrd f (a, b, c) =   a,   b, f c


