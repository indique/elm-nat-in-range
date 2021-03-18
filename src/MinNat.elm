module MinNat exposing
    ( MinNat
    , atMost, abs, intAtLeast
    , isIntAtLeast, is, isAtLeast, isAtMost, theGreater, theSmaller
    , toInt, n, lowerMin
    , addN, subN, add, subIn, mul, div, remainderBy, toPower
    , range
    )

{-| A Nat which is at least some minimum.

@docs MinNat


## add information

@docs atMost, abs, intAtLeast


### compare

@docs isIntAtLeast, is, isAtLeast, isAtMost, theGreater, theSmaller


## drop information

@docs toInt, n, lowerMin


## modify

@docs addN, subN, add, subIn, mul, div, remainderBy, toPower


## extra

@docs range

-}

import InNat as InNat exposing (InNat)
import Internal
import N.Nat.Type exposing (..)
import N.Type exposing (..)
import NNat exposing (NNat)


{-| `NatFrom min` meaning a natural number which is at least `min`.

       ↓ minimum
    ⨯ [✓ ✓ ✓ ✓ ✓ ✓ ✓...

Any natural number

    MinNat Nat0

A number, bigger than 4

    MinNat Nat5

-}
type alias MinNat min =
    Internal.MinNat min


newMin : MinNat min -> MinNat newMin
newMin =
    map identity


map : (Int -> Int) -> MinNat min -> MinNat resultMin
map update =
    toInt >> update >> Internal.MinNat



--## drop information


{-| Convert a `MinNat` to an `Int`.

    MinNat.n nat4 |> MinNat.toInt --> 4

    compare a b =
        compare (MinNat.toInt a) (MinNat.toInt b)

-}
toInt : MinNat min -> Int
toInt =
    Internal.minNatToInt


{-| Convert an exact `NNat n` to a `MinNat n (n + a)`.

    MinNat.n nat4
    --> is of type MinNat Nat4 (Nat4Plus a)

-}
n : NNat (N n Is difference) -> MinNat n
n nNat =
    NNat.toInt nNat |> Internal.MinNat


{-| Set the minimum lower.
-}
lowerMin :
    NNat (N alsoValidmin Is (Difference currentMinusAlsoValidmin To min))
    -> MinNat min
    -> MinNat alsoValidmin
lowerMin alsoValidmin =
    newMin



--## add information


{-| **Cap** the `MinNat` to at most a upper limit.
-}
atMost :
    NNat (N max Is (Difference a To maxPlusA))
    -> { min : NNat (N min Is (Difference range To max)) }
    -> MinNat min
    -> InNat min maxPlusA
atMost max min =
    isAtMost max
        min
        { greater = \_ -> max |> NNat.toInt |> Internal.InNat
        , equalOrLess = identity
        }


{-| The absolute value of an `Int`, which is at least `Nat0`.

    MinNat.abs 16 --> MinNat 16

    MinNat.abs -4 --> MinNat 4

Really only use this if you want the absolute value.

    badLength list =
        List.length >> MinNat.abs

    goodLength =
        List.foldl
            (\_ -> MinNat.addN nat1 >> MinNat.lowerMin nat0)
            (MinNat.n nat0)

If something like this isn't possible, use `[MinNat.intAtLeast][MinNat#intAtLeast] nat0`!

-}
abs : Int -> MinNat Nat0
abs int =
    Basics.abs int |> Internal.MinNat


{-| If the `Int >= a minimum`, `Just the MinNat`, else `Nothing`.

    4 |> MinNat.isIntAtLeast nat5 --> Nothing

    1234 |> MinNat.isIntAtLeast nat5 --> Just (NatIn 1234)

-}
isIntAtLeast : NNat (N min Is difference) -> Int -> Maybe (MinNat n)
isIntAtLeast min int =
    if int >= NNat.toInt min then
        Just (Internal.MinNat int)

    else
        Nothing


{-| A `MinNat` from an `Int`; if the `Int < minimum`, `minimum` is returned.

    9 |> MinNat.intAtLeast nat3 --> MinNat 9

    0 |> MinNat.intAtLeast nat3 --> MinNat 3

You can also use this if you know an `Int` must be at least `minimum`.

But avoid it if you can do better, like

    goodLength =
        List.foldl
            (\_ -> MinNat.addN nat1 >> MinNat.lowerMin nat0)
            (MinNat.n nat0)

If you want to handle the case `< minimum` yourself, use [`MinNat.isIntAtLeast`][MinNat#isIntAtLeast].

-}
intAtLeast : NNat (N n Is difference) -> Int -> MinNat n
intAtLeast minimum int =
    isIntAtLeast minimum int
        |> Maybe.withDefault (n minimum)



-- ## modify


{-| Add a `MinNat`. The second argument is the minimum if the added `MinNat`.

    (nat5 |> MinNat.n)
        |> MinNat.add atLeast2 nat2

-}
add :
    MinNat addedMin
    -> NNat (N addedMin Is (Difference min To summin))
    -> MinNat min
    -> MinNat summin
add addedMinNat addedMin =
    map ((+) (toInt addedMinNat))


{-| Add an exact `NNat`.

    (nat5 |> MinNat.n)
        |> MinNat.addN nat2
    --> MinNat 7

-}
addN :
    NNat (N n Is (Difference min To minPlusN))
    -> MinNat min
    -> MinNat minPlusN
addN nNat =
    map ((+) (NNat.toInt nNat))


{-| Subtract a `InNat`. The second argument is the maximum if the subtracted `InNat`.

    (nat5 |> MinNat.n)
        |> MinNat.subIn inNat0To5 itsMaximum

-}
subIn :
    InNat subtractedMin subtractedMax
    -> NNat (N subtractedMax Is (Difference differenceMin To min))
    -> MinNat min
    -> MinNat differenceMin
subIn subtractedInNat subtractedMax =
    map (\base -> base - Internal.inNatToInt subtractedInNat)


{-| Subtract an exact `NNat`.

    (nat7 |> MinNat.n)
        |> MinNat.subN nat2
    --> MinNat 5

-}
subN :
    NNat (N n Is (Difference minMinusN To min))
    -> MinNat min
    -> MinNat minMinusN
subN nNat =
    map (\base -> base - NNat.toInt nNat)


{-| Multiply by a `MinNat` >= 1.
We can't compute the highest possiblenew minimum,
we only know that if `a >= 1  →  x * a >= x`

    five |> MinNat.mul two
    --> MinNat 10, but the type is MinNat Nat5

    two |> MinNat.mul five
    --> MinNat 10, but the type is MinNat Nat2

    two = nat2 |> MinNat.n

    five = nat5 |> MinNat.n

-}
mul :
    MinNat (Nat1Plus multipliedMinMinus1)
    -> MinNat min
    -> MinNat min
mul minNat =
    map ((*) (toInt minNat))


{-| Divide (`//`) by a `MinNat`. `div 0` is impossible.

    MinNat.n nat7 |> MinNat.div (MinNat.n nat3)
    --> MinNat 2 of type MinNat Nat0

-}
div :
    MinNat (Nat1Plus minMinus1)
    -> MinNat min
    -> MinNat Nat0
div minNat =
    map (\base -> base // toInt minNat)


{-| The remainder after division. `remainderBy 0` is impossible.

    MinNat.n nat7 |> MinNat.remainderBy (MinNat.n nat3)
    --> MinNat Nat0

-}
remainderBy :
    MinNat (Nat1Plus dMinMinus1)
    -> MinNat min
    -> MinNat Nat0
remainderBy minNat =
    map (Basics.remainderBy (minNat |> Internal.minNatToInt))


{-| The `MinNat ^ a MinNat`.
We can't compute the highest possible new minimum,
we only know that if `a >= 1  →  x ^ a >= x`

    five |> MinNat.toPower two
    --> MinNat 25, but the type is MinNat Nat5

    two |> MinNat.mul five
    --> MinNat 25, but the type is MinNat Nat2

    two = nat2 |> MinNat.n

    five = nat5 |> MinNat.n

-}
toPower :
    MinNat (Nat1Plus minMinus1)
    -> MinNat min
    -> MinNat min
toPower power =
    \minNat -> toInt minNat ^ toInt power |> Internal.MinNat



-- ## compare


{-| Compared to an exact `NNat`.
-}
is :
    NNat (N (Nat1Plus triedMinus1) Is (Difference (Nat1Plus aMinus1) To (Nat1Plus triedMinus1PlusA)))
    -> { min : NNat (N min Is (Difference lessRange To triedMinus1)) }
    ->
        { equal : () -> result
        , less : InNat min triedMinus1PlusA -> result
        , greater : MinNat (Nat2Plus triedMinus1) -> result
        }
    -> MinNat min
    -> result
is tried min cases =
    \minNat ->
        case compare (toInt minNat) (NNat.toInt tried) of
            EQ ->
                .equal cases ()

            LT ->
                .less cases (toInt minNat |> Internal.InNat)

            GT ->
                .greater cases (newMin minNat)


{-| Is the `MinNat`

  - `equalOrGreater` than a `NNat` or

  - `less`?

```
factorialHelp : MinNat min -> MinNat Nat1
factorialHelp =
    MinNat.lowerMin nat0
        >> MinNat.isAtLeast nat1
            { min = nat0 }
            { less = \_ -> MinNat.n nat1
            , equalOrGreater =
                \oneOrGreater ->
                    oneOrGreater
                        |> MinNat.mul
                            (factorialHelp
                                (oneOrGreater |> MinNat.subN nat1)
                            )
            }
```

-}
isAtLeast :
    NNat (N triedMin Is (Difference a To (Nat1Plus triedMinMinus1PlusA)))
    -> { min : NNat (N min Is (Difference (Nat1Plus lessRange) To triedMin)) }
    ->
        { less : InNat min triedMinMinus1PlusA -> result
        , equalOrGreater : MinNat triedMin -> result
        }
    -> MinNat min
    -> result
isAtLeast triedLowerLimit min cases =
    \minNat ->
        if toInt minNat >= NNat.toInt triedLowerLimit then
            .equalOrGreater cases (newMin minNat)

        else
            .less cases (toInt minNat |> Internal.InNat)


{-| Is the `MinNat`

  - `equalOrLess` than a `NNat` or

  - `greater`?

-}
isAtMost :
    NNat (N atMostMin Is (Difference a To atMostMinPlusA))
    -> { min : NNat (N min Is (Difference minToAtMostMin To atMostMin)) }
    ->
        { equalOrLess : InNat min atMostMinPlusA -> result
        , greater : MinNat atMostMin -> result
        }
    -> MinNat min
    -> result
isAtMost triedUpperLimit min cases =
    \minNat ->
        if toInt minNat <= NNat.toInt triedUpperLimit then
            .equalOrLess cases (toInt minNat |> Internal.InNat)

        else
            .greater cases (newMin minNat)


{-| The greater of 2 `MinNat`s. Works just like [Basics.max][Basics#max].

    MinNat.theGreater
        (nat3 |> MinNat.n)
        (nat4 |> MinNat.n |> MinNat.lowerMin nat3)
    --> MinNat 4

-}
theGreater : MinNat min -> MinNat min -> MinNat min
theGreater a b =
    Basics.max (toInt a) (toInt b)
        |> Internal.MinNat


{-| The smaller of 2 `MinNat`s. Works just like [Basics.min][Basics#min].

    MinNat.theSmaller
        (nat3 |> MinNat.n)
        (nat4 |> MinNat.n |> MinNat.lowerMin nat3)
    --> MinNat 3

-}
theSmaller : MinNat min -> MinNat min -> MinNat min
theSmaller a b =
    Basics.min (toInt a) (toInt b)
        |> Internal.MinNat



-- ## extra


{-| `MinNat`s from a first `InNat` to a last `MinNat`.
-}
range :
    InNat firstMin lastMin
    -> MinNat lastMin
    -> List (MinNat firstMin)
range first last =
    List.range (InNat.toInt first) (toInt last)
        |> List.map Internal.MinNat
