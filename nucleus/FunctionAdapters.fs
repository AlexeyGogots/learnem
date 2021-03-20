namespace global

[<AutoOpen>]
module FunctionAdapters =

    /// Creates a new function with first two arguments flipped
    let inline flip f x y = f y x

    /// Creates a new function that returns the given constant
    let inline konst x = (fun _ -> x)

    /// Converts a function expecting a tuple of arguments into its curried form
    let inline curry f x y = f (x, y)

    /// Converts a function taking 2 curried arguments into a function taking a single tuple
    let inline uncurry f (a,b) = f a b

    /// Converts a function taking 3 curried arguments into a function taking a single tuple
    let inline uncurry3 f (a,b,c) = f a b c


