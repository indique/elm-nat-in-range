module InNat exposing
    ( InNat
    , add
    , addN
    , atLeast
    , atMost
    , equalOrLessThan
    , fromAbsAtMost
    , fromBetween
    , fromN
    , generate
    , isMin
    , maxAtLeast
    , range
    , sub
    , subN
    , toInt
    , vs
    )

import Internal as Internal exposing (InNat(..))
import N.Nat.Type exposing (..)
import N.Type exposing (..)
import NNat as NNat exposing (NNat)
import Random


{-| For every constructable `InNat`, maximum will be at least as high as minimum.
-}
type alias InNat minimum maximum =
    Internal.InNat minimum maximum



--## transform


toInt : InNat minimum maximum -> Int
toInt =
    \(InNat int) -> int



-- ## az most, at least


atLeast :
    NNat (N newMinimum Is difference)
    -> NNat (N oldMinimum Is (Difference oldMinusNewMinimum To newMinimum))
    -> InNat oldMinimum maximum
    -> InNat newMinimum maximum
atLeast newMinimum oldMinimum =
    \inRange ->
        InNat (max (NNat.toInt newMinimum) (toInt inRange))


{-| Guaranteed.
-}
isMin :
    NNat (N alsoValidMinimum Is (Difference currentMinusAlsoValidMinimum To minimum))
    -> InNat minimum maximum
    -> InNat alsoValidMinimum maximum
isMin alsoValidMinimum =
    \inRange -> InNat (toInt inRange)


{-| Guaranteed. Used when the required maximum is higher.
-}
maxAtLeast :
    NNat (N maximum Is (Difference a To maximumPlusA))
    -> InNat minimum maximum
    -> InNat minimum maximumPlusA
maxAtLeast maximum =
    \inRange -> InNat (toInt inRange)


{-| The maximum can be bigger than the current maximum.
-}
atMost :
    NNat (N newMaximum Is (Difference a To newMaximumPlusA))
    -> NNat (N minimum Is (Difference newRange To newMaximum))
    -> InNat minimum oldMaximum
    -> InNat minimum newMaximumPlusA
atMost maximum maximumMustBeAtLeastMinimum =
    \inRange ->
        InNat (min (NNat.toInt maximum) (toInt inRange))



-- ## create


fromN :
    NNat (N n Is (Difference a To nPlusA))
    -> InNat n nPlusA
fromN nnat =
    InNat (NNat.toInt nnat)


fromAbsAtMost :
    NNat (N maximum Is (Difference a To maximumPlusA))
    -> Int
    -> InNat N0Nat maximumPlusA
fromAbsAtMost maximum int =
    InNat (abs int)
        |> atMost maximum NNat.n0


fromBetween :
    NNat (N minimum Is (Difference range To maximum))
    -> NNat (N maximum Is (Difference a To maximumPlusA))
    -> Int
    -> InNat minimum maximumPlusA
fromBetween minimum maximum int =
    InNat (max (NNat.toInt minimum) int)
        |> atMost maximum minimum


range :
    { first : NNat (N first Is (Difference range To last))
    , last : NNat (N last Is (Difference a To lastPlusA))
    }
    -> List (InNat first lastPlusA)
range interval =
    List.range
        (NNat.toInt (.first interval))
        (NNat.toInt (.last interval))
        |> List.map InNat


{-| -}
generate :
    NNat (N minimum Is (Difference range To maximum))
    -> NNat (N maximum Is (Difference a To maximumPlusA))
    -> Random.Generator (InNat minimum maximumPlusA)
generate minimum maximum =
    Random.int (NNat.toInt minimum) (NNat.toInt maximum)
        |> Random.map InNat



-- ## mutate


add :
    InNat addedMinimum addedMaximum
    -> NNat (N addedMinimum Is (Difference minimum To sumMinimum))
    -> NNat (N addedMaximum Is (Difference maximum To sumMaximum))
    -> InNat minimum maximum
    -> InNat sumMinimum sumMaximum
add added addedMinimum addedMaximum =
    \inRange ->
        InNat (toInt inRange + toInt added)


addN :
    ( NNat (N added Is (Difference minimum To sumMinimum))
    , NNat (N added Is (Difference maximum To sumMaximum))
    )
    -> InNat minimum maximum
    -> InNat sumMinimum sumMaximum
addN addedNNat =
    \inRange ->
        InNat (toInt inRange + NNat.toInt (addedNNat |> Tuple.first))


sub :
    InNat subtractedMinimum subtractedMaximum
    -> NNat (N subtractedMinimum Is (Difference differenceMaximum To maximum))
    -> NNat (N subtractedMaximum Is (Difference differenceMinimum To minimum))
    -> InNat minimum maximum
    -> InNat differenceMinimum differenceMaximum
sub subtracted subtractedMinimum subtractedMaximum =
    \inRange ->
        InNat (toInt inRange - toInt subtracted)


subN :
    ( NNat (N subtracted Is (Difference differenceMinimum To minimum))
    , NNat (N subtracted Is (Difference differenceMaximum To maximum))
    )
    -> InNat minimum maximum
    -> InNat differenceMinimum differenceMaximum
subN subtractedNNat =
    \inRange ->
        InNat (toInt inRange - NNat.toInt (subtractedNNat |> Tuple.first))



-- ## scan


equalOrLessThan :
    ( NNat (N tried Is (Difference (N1NatPlus triedToMaximumMinus1) To maximum))
    , NNat (N tried Is (Difference a To triedPlusA))
    )
    -> NNat (N minimum Is (Difference minimumToTried To tried))
    ->
        { equalOrLess : InNat minimum triedPlusA -> result
        , greater : InNat (N1NatPlus tried) maximum -> result
        }
    -> InNat minimum maximum
    -> result
equalOrLessThan tried minimum cases =
    \inRange ->
        if toInt inRange <= NNat.toInt (tried |> Tuple.first) then
            .equalOrLess cases
                (InNat (toInt inRange))

        else
            .greater cases
                (InNat (toInt inRange))


equalOrGreaterThan :
    ( NNat (N (N1NatPlus triedMinus1) Is (Difference triedToMaximum To maximum))
    , NNat (N (N1NatPlus triedMinus1) Is (Difference a To (N1NatPlus triedMinus1PlusA)))
    )
    -> NNat (N minimum Is (Difference minimumToTried To (N1NatPlus triedMinus1)))
    ->
        { less : InNat minimum (N1NatPlus triedMinus1PlusA) -> result
        , greaterOrEqual : InNat (N1NatPlus triedMinus1) maximum -> result
        }
    -> InNat minimum maximum
    -> result
equalOrGreaterThan tried minimum cases =
    \inRange ->
        if toInt inRange >= NNat.toInt (tried |> Tuple.first) then
            .greaterOrEqual cases
                (InNat (toInt inRange))

        else
            .less cases
                (InNat (toInt inRange))


vs :
    ( NNat (N (N1NatPlus triedMinus1) Is (Difference (N1NatPlus triedToMaximumMinus1) To maximum))
    , NNat (N (N1NatPlus triedMinus1) Is (Difference a To (N1NatPlus triedPlusAMinus1)))
    )
    -> NNat (N minimum Is (Difference minimumToTriedMinus1 To triedMinus1))
    ->
        { equal : () -> result
        , less : InNat minimum triedPlusAMinus1 -> result
        , greater : InNat (N2NatPlus triedMinus1) maximum -> result
        }
    -> InNat minimum maximum
    -> result
vs tried minimum cases =
    \inRange ->
        case compare (toInt inRange) (NNat.toInt (tried |> Tuple.first)) of
            EQ ->
                .equal cases ()

            GT ->
                .greater cases
                    (InNat (toInt inRange))

            LT ->
                .less cases
                    (InNat (toInt inRange))
