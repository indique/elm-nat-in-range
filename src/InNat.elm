module InNat exposing
    ( InNat
    , intInRange, atLeast, atMost
    , is, isInRange, isIntInRange, isAtLeast, isAtMost
    , addN, subN, add, sub, div, remainderBy
    , n, toInt, lowerMin, maxIs, dropMax
    , random, range
    )

{-| A natural number within a minimum & maximum.

@docs InNat


## add information


### clamp

@docs intInRange, atLeast, atMost


### compare

@docs is, isInRange, isIntInRange, isAtLeast, isAtMost


## modify

@docs addN, subN, add, sub, div, remainderBy

If you want other operations like multiply, use [dropMax][InNat#dropMax].


## drop information

@docs n, toInt, lowerMin, maxIs, dropMax


## more

@docs random, range

-}

import Internal exposing (MinNat)
import N.Nat.Type exposing (..)
import N.Type exposing (..)
import NNat exposing (NNat)
import NNats exposing (..)
import Random


{-| A natural number somewhere within a minimum & maximum.

       ↓ minimum    ↓ maximum
    ⨯ [✓ ✓ ✓ ✓ ✓ ✓ ✓] ⨯ ⨯ ⨯...

Note: maximum >= minimum for every existing `InNat`:

    percent : InNat minimum Nat100

→ `minimum <= Nat100`

-}
type alias InNat minimum maximum =
    Internal.InNat minimum maximum


{-| **Never expose this**.
-}
newRange : InNat min max -> InNat newMin newMax
newRange =
    map identity


map : (Int -> Int) -> InNat min max -> InNat resultMin resultMax
map update =
    toInt >> update >> Internal.InNat



-- ## add information
--### clamp


{-| A `InNat` from an `Int`, **clamped** between a `minimum` & `maximum`.

  - if the `Int < minimum`, `minimum` is returned
  - if the `Int > maximum`, `maximum` is returned

```
9 |> InNat.intInRange nat3 nat12 --> InNat 9

0 |> InNat.intInRange nat3 nat12 --> InNat 3

99 |> InNat.intInRange nat3 nat12 --> InNat 12
```

If you want to handle the cases `< minimum` & `> maximum` explicitly, use [`isIntInRange`][InNat#isIntInRange]

-}
intInRange :
    NNat (N min Is (Difference range To max))
    -> NNat (N max Is (Difference a To maxPlusA))
    -> Int
    -> InNat min maxPlusA
intInRange lowerLimit upperLimit =
    isIntInRange { first = lowerLimit, last = upperLimit }
        { less = \_ -> n lowerLimit |> maxIs upperLimit
        , greater = \_ -> n upperLimit |> lowerMin lowerLimit
        , inRange = identity
        }


{-| **Cap** the `InNat` to at most a number.

    InNat.n nat5
        |> InNat.atMost nat10 { min = nat5 }
    --> InNat 5

    InNat.n nat15
        |> InNat.lowerMin nat5
        |> InNat.atMost nat10 { min = nat5 }
    --> InNat 10

`min` ensures that that number is at least the minimum.

-}
atMost :
    NNat (N newMax Is (Difference a To newMaxPlusA))
    -> { min : NNat (N min Is (Difference newRange To newMax)) }
    -> InNat min oldMax
    -> InNat min newMaxPlusA
atMost higherLimit min =
    map (Basics.min (NNat.toInt higherLimit))


{-| If the `InNat` is less than a number, return that number instead.

    InNat.n nat5
        |> InNat.atLeast nat10 { min = nat5 }
    --> InNat 10

    InNat.n nat15
        |> InNat.lowerMin nat5
        |> InNat.atLeast nat10 { min = nat5 }
    --> InNat 15

-}
atLeast :
    NNat (N newMin Is difference)
    -> { min : NNat (N oldMin Is (Difference oldMinusNewMin To newMin)) }
    -> InNat oldMin max
    -> InNat newMin max
atLeast lowerLimit oldMin =
    map (max (NNat.toInt lowerLimit))



-- ## compare


{-| An `Int` compared to a range from `first` to `last`.

    rejectOrAcceptUserInt =
        isIntInRange { first = nat1, last = nat100 }
            { less = Err "must be >= 1"
            , more = Err "must be <= 100"
            , inRange = Ok
            }

    rejectOrAcceptUserInt (InNat.n nat0)
    --> Err "must be >= 1"

-}
isIntInRange :
    { first : NNat (N first Is (Difference range To last))
    , last : NNat (N last Is (Difference a To lastPlusA))
    }
    ->
        { less : () -> result
        , greater : MinNat (Nat1Plus last) -> result
        , inRange : InNat first lastPlusA -> result
        }
    -> Int
    -> result
isIntInRange interval cases int =
    if int < NNat.toInt (.first interval) then
        .less cases ()

    else if int > NNat.toInt (.last interval) then
        .greater cases (int |> Internal.MinNat)

    else
        .inRange cases (int |> Internal.InNat)


{-| Is the `InNat`

  - `equalOrGreater` than a `NNat` or

  - `less`?

```
vote : { a | age : InNat (Nat18Plus orOlder) max } -> Vote

tryToVote =
    InNat.lowerMin nat0
        >> InNat.isAtLeast ( nat18, nat18 )
            { min = nat0 }
            { less = Nothing --😓
            , equalOrGreater = Just << vote
            }
```

-}
isAtLeast :
    ( NNat (N tried Is (Difference a To (Nat1Plus triedMinus1PlusA)))
    , NNat (N tried Is (Difference atLeastRange To max))
    )
    -> { min : NNat (N min Is (Difference (Nat1Plus lessRange) To tried)) }
    ->
        { less : InNat min triedMinus1PlusA -> result
        , equalOrGreater : InNat tried max -> result
        }
    -> InNat min max
    -> result
isAtLeast triedLowerLimit min cases =
    \inNat ->
        if toInt inNat >= NNat.toInt (triedLowerLimit |> Tuple.first) then
            .equalOrGreater cases (newRange inNat)

        else
            .less cases (newRange inNat)


{-| Is the `InNat`

  - `equalOrLess` than a `NNat` or

  - `greater`?

```
goToU18Party : { a | age : InNat min Nat17 } -> Snacks

tryToGoToU18Party =
    InNat.lowerMin nat0
        >> InNat.isAtMost ( nat17, nat17 )
            { min = nat0 }
            { equalOrLess = Just << goToU18Party
            , greater = Nothing
            }
```

-}
isAtMost :
    ( NNat (N tried Is (Difference a To triedPlusA))
    , NNat (N tried Is (Difference (Nat1Plus greaterRange) To max))
    )
    -> { min : NNat (N min Is (Difference atMostRange To tried)) }
    ->
        { equalOrLess : InNat min triedPlusA -> result
        , greater : InNat tried max -> result
        }
    -> InNat min max
    -> result
isAtMost triedUpperLimit min cases =
    \inNat ->
        if toInt inNat <= NNat.toInt (triedUpperLimit |> Tuple.first) then
            .equalOrLess cases (newRange inNat)

        else
            .greater cases (newRange inNat)


{-| Compare the `InNat` to a `NNat`. Is it greater, less or equal to that `NNat`?

`min` ensures that the `NNat` is bigger than the minimum.

    justIfGreater4 =
        InNat.minIs nat0
            >> is ( nat4, nat4 )
                { min = nat0 }
                { greater = Just
                , less = \_ -> Nothing
                , equal = \() -> Nothing
                }


    justIfGreater4 (InNat.n nat5)
    --> Just (InNat 5)

    justIfGreater4 (InNat.n nat3)
    --> Nothing

-}
is :
    ( NNat (N tried Is (Difference (Nat1Plus greaterRange) To max))
    , NNat (N tried Is (Difference a To (Nat1Plus triedPlusAMinus1)))
    )
    -> { min : NNat (N min Is (Difference (Nat1Plus lessRange) To tried)) }
    ->
        { equal : () -> result
        , less : InNat min triedPlusAMinus1 -> result
        , greater : InNat (Nat2Plus triedMinus1) max -> result
        }
    -> InNat min max
    -> result
is tried min cases =
    \inNat ->
        case compare (toInt inNat) (NNat.toInt (tried |> Tuple.first)) of
            EQ ->
                .equal cases ()

            GT ->
                .greater cases (newRange inNat)

            LT ->
                .less cases (newRange inNat)


{-| Compared to a range `first` to `last`, is the `InNat`

  - `inRange`

  - `greater` than the `last` or

  - `less` than the `first`?

```
justIfBetween3And10 =
    InNat.lowerMin nat0
        >> InNat.isInRange
            { first = ( nat3, nat3 ), last = ( nat10, nat10 ) }
            { min : nat0 }
            { less = \_ -> Nothing
            , greater = \_ -> Nothing
            , inRange = Just
            }

justIfBetween3And10 (InNat.n nat9)
--> Just (InNat 9)

justIfBetween3And10 (InNat.n nat123)
--> Nothing
```

-}
isInRange :
    { first :
        ( NNat (N first Is (Difference range To last))
        , NNat (N first Is (Difference a To (Nat1Plus firstMinus1PlusA)))
        )
    , last :
        ( NNat (N last Is (Difference (Nat1Plus greaterRange) To max))
        , NNat (N last Is (Difference a To lastPlusA))
        )
    }
    -> { min : NNat (N min Is (Difference (Nat1Plus lessRange) To first)) }
    ->
        { inRange : InNat first lastPlusA -> result
        , less : InNat min firstMinus1PlusA -> result
        , greater : InNat (Nat1Plus last) max -> result
        }
    -> InNat min max
    -> result
isInRange interval min cases =
    \inNat ->
        let
            firstInt =
                .first interval |> Tuple.first |> NNat.toInt

            lastInt =
                .last interval |> Tuple.first |> NNat.toInt
        in
        if toInt inNat < firstInt then
            .less cases (newRange inNat)

        else if toInt inNat > lastInt then
            .greater cases (newRange inNat)

        else
            .inRange cases (newRange inNat)



-- ## modify


{-| Add a `InNat`.

    InNat.n nat3
        |> InNat.add between1And12 nat1 nat12
    --> of type InNat Nat4 (Nat15Plus a)

-}
add :
    InNat addedMin addedMax
    -> NNat (N addedMin Is (Difference min To sumMin))
    -> NNat (N addedMax Is (Difference max To sumMax))
    -> InNat min max
    -> InNat sumMin sumMax
add added addedMin addedMax =
    map (\inNat -> inNat + toInt added)


{-| Add a fixed `NNat` value.

    InNat.n nat70
        |> InNat.addN ( nat7, nat7 )
    --> is of type InNat Nat77 (Nat77Plus a)

-}
addN :
    ( NNat (N added Is (Difference min To sumMin))
    , NNat (N added Is (Difference max To sumMax))
    )
    -> InNat min max
    -> InNat sumMin sumMax
addN addedNNat =
    map (\inNat -> inNat + NNat.toInt (addedNNat |> Tuple.first))


{-| Subtract a `InNat`.

    InNat.n nat6
        |> InNat.sub between1And5 nat1 nat5
    --> is of type InNat Nat1 (Nat5Plus a)

-}
sub :
    InNat subtractedMin subtractedMax
    -> NNat (N subtractedMin Is (Difference differenceMax To max))
    -> NNat (N subtractedMax Is (Difference differenceMin To min))
    -> InNat min max
    -> InNat differenceMin differenceMax
sub subtractedInNat subtractedMin subtractedMax =
    map (\inNat -> inNat - toInt subtractedInNat)


{-| Subtract a fixed `NNat` value.

    InNat.n nat7
        |> InNat.subN ( nat7, nat7 )
    --> InNat.n nat0

-}
subN :
    ( NNat (N subtracted Is (Difference differenceMin To min))
    , NNat (N subtracted Is (Difference differenceMax To max))
    )
    -> InNat min max
    -> InNat differenceMin differenceMax
subN subtractedNNat =
    map
        (\inNat ->
            inNat - NNat.toInt (subtractedNNat |> Tuple.first)
        )


{-| Divide (`//`) by a `MinNat`. `div 0` is impossible.

    InNat.n nat7 |> InNat.div (MinNat.n nat3)
    --> InNat 2 of type InNat Nat0 (Nat7Plus a)

-}
div :
    MinNat (Nat1Plus dMinMinus1)
    -> InNat min max
    -> InNat Nat0 max
div minNat =
    map (\base -> base // Internal.minNatToInt minNat)


{-| The remainder after division. `remainderBy 0` is impossible.

    InNat.n nat7 |> InNat.remainderBy (MinNat.n nat3)
    --> InNat Nat0 (Nat7Plus a)

-}
remainderBy :
    MinNat (Nat1Plus dMinMinus1)
    -> InNat min max
    -> InNat Nat0 max
remainderBy minNat =
    map (Basics.remainderBy (minNat |> Internal.minNatToInt))



--## drop information


{-| Convert the value in a `InNat` to an `Int`.
-}
toInt : InNat min max -> Int
toInt =
    Internal.inNatToInt


{-| Set the minimum lower.

    [ InNat.n nat3, InNat.n nat4 ]

Elm complains:

> But all the previous elements in the list are: `InNat Nat3 ...`

    [ InNat.n nat3
    , InNat.n nat4 |> InNat.lowerMin nat3
    ]

-}
lowerMin :
    NNat (N lowerMin Is (Difference currentMinusAlsoValidmin To min))
    -> InNat min max
    -> InNat lowerMin max
lowerMin lowerMinimum =
    newRange


{-| Set the maximum higher.

You should design type annotations as general as possible.

    onlyAtMost18 : InNat min Nat18

    onlyAtMost18 between3And8 --fine

But once you implement `onlyAtMost18`, you might use the value in `onlyAtMost19`.

    onlyAtMost18 value =
        -- onlyAtMost19 value --error :(
        onlyAtMost19
            (value |> InNat.maxIs nat18 {- works :) -})

-}
maxIs :
    NNat (N max Is (Difference a To maxPlusA))
    -> InNat min max
    -> InNat min maxPlusA
maxIs max =
    newRange


{-| A `InNat` from an exact `NNat n`.

The minimum is `n`, the maximum `>= n`.

-}
n :
    NNat (N n Is (Difference a To nPlusA))
    -> InNat n nPlusA
n nNat =
    nNat |> NNat.toInt |> Internal.InNat


{-| Convert a `InNat` to a `MinNat`.

    squareAInNat =
        MinNat.dropMax >> MinNat.toPower (MinNat.n nat2)

-}
dropMax : InNat min max -> MinNat min
dropMax =
    toInt >> Internal.MinNat



-- ## more


{-| `InNat`s from a first to a last value.

    from3To10 =
        InNat.range (nat3 |> InNat.n) (nat10 |> InNat.n)

-}
range :
    InNat firstMin lastMin
    -> InNat lastMin lastMax
    -> List (InNat firstMin lastMax)
range first last =
    List.range (toInt first) (toInt last)
        |> List.map Internal.InNat


{-| Generate a random `InNat` in a range.
-}
random :
    NNat (N min Is (Difference range To max))
    -> NNat (N max Is (Difference a To maxPlusA))
    -> Random.Generator (InNat min maxPlusA)
random min max =
    Random.int (NNat.toInt min) (NNat.toInt max)
        |> Random.map Internal.InNat
