module MinNat exposing
    ( MinNat
    , add
    , addN
    , div
    , equalOrLessThan
    , fromAbs
    , fromAtLeast
    , fromIn
    , fromN
    , isMin
    , mul
    , sub
    , subN
    , toInt
    , toPower
    , vs
    )

import InNat as InNat exposing (InNat)
import Internal as Internal
import N.Nat.Type exposing (..)
import N.Type exposing (..)
import NNat exposing (NNat)


{-| `NatFrom minimum` meaning a natural number which is at least `minimum`.

Any natural number

    NatFrom N0Nat

A number, bigger than 4

    NatFrom N5Nat

-}
type MinNat minimum
    = NatFrom Int



--## transform


toInt : MinNat minimum -> Int
toInt natAtLeast =
    natAtLeast |> (\(NatFrom int) -> int)



-- ## create


fromAbs : Int -> MinNat N0Nat
fromAbs int =
    abs int |> NatFrom


fromAtLeast : NNat (N n Is difference) -> Int -> MinNat n
fromAtLeast minimum int =
    max (NNat.toInt minimum) int |> NatFrom


fromN : NNat (N n Is difference) -> MinNat n
fromN nNat =
    NNat.toInt nNat |> NatFrom


fromIn : InNat minimum maximum -> MinNat minimum
fromIn natIn =
    InNat.toInt natIn |> NatFrom



-- ## scan


{-| Guaranteed.
-}
isMin :
    NNat (N alsoValidMinimum Is (Difference currentMinusAlsoValidMinimum To minimum))
    -> MinNat minimum
    -> MinNat alsoValidMinimum
isMin alsoValidMinimum =
    \inRange -> NatFrom (toInt inRange)



-- ## modify


add :
    MinNat addedMinimum
    -> NNat (N addedMinimum Is (Difference minimum To sumMinimum))
    -> MinNat minimum
    -> MinNat sumMinimum
add natAtLeast addedMinimum =
    \atLeast -> toInt atLeast + toInt natAtLeast |> NatFrom


addN :
    NNat (N n Is (Difference minimum To minimumPlusN))
    -> MinNat minimum
    -> MinNat minimumPlusN
addN nNat =
    \atLeast -> toInt atLeast + NNat.toInt nNat |> NatFrom


sub :
    MinNat subtractedMinimum
    -> NNat (N subtractedMinimum Is (Difference differenceMinimum To minimum))
    -> MinNat minimum
    -> MinNat differenceMinimum
sub natAtLeast subtractedMinimum =
    \atLeast -> toInt atLeast - toInt natAtLeast |> NatFrom


subN :
    NNat (N n Is (Difference minimumMnusN To minimum))
    -> MinNat minimum
    -> MinNat minimumMinusN
subN nNat =
    \atLeast -> toInt atLeast - NNat.toInt nNat |> NatFrom


mul2 :
    NNat (N minimum Is (Difference minimum To minimumTimes2))
    -> MinNat minimum
    -> MinNat minimumTimes2
mul2 minimum =
    \atLeast -> toInt atLeast * 2 |> NatFrom


mul :
    MinNat (N1NatPlus multipliedMinimumMinus1)
    -> MinNat minimum
    -> MinNat minimum
mul natAtLeast =
    \atLeast -> toInt atLeast * toInt natAtLeast |> NatFrom


div :
    MinNat (N1NatPlus minimumMinus1)
    -> MinNat minimum
    -> MinNat N0Nat
div natAtLeast =
    \atLeast -> toInt atLeast // toInt natAtLeast |> NatFrom


toPower :
    MinNat (N1NatPlus minimumMinus1)
    -> MinNat minimum
    -> MinNat minimum
toPower power =
    \atLeast -> toInt atLeast ^ toInt power |> NatFrom



-- ## scan


vs :
    ( NNat (N (N1NatPlus triedMinus1) Is (Difference (N1NatPlus triedToMinimumMinus1) To minimum))
    , NNat (N (N1NatPlus triedMinus1) Is (Difference (N1NatPlus aMinus1) To (N1NatPlus triedMinus1PlusA)))
    )
    ->
        { equal : () -> result
        , less : InNat minimum triedMinus1PlusA -> result
        , greater : MinNat (N2NatPlus triedMinus1) -> result
        }
    -> MinNat minimum
    -> result
vs tried cases =
    \atLeast ->
        case compare (toInt atLeast) (NNat.toInt (tried |> Tuple.first)) of
            EQ ->
                .equal cases ()

            LT ->
                .less cases (toInt atLeast |> Internal.InNat)

            GT ->
                .greater cases (toInt atLeast |> NatFrom)


equalOrGreaterThan :
    NNat (N tried Is (Difference a To (N1NatPlus triedMinus1PlusA)))
    -> NNat (N minimum Is (Difference triedToMinimum To tried))
    ->
        { equalOrGreater : MinNat (N1NatPlus tried) -> result
        , less : InNat minimum triedMinus1PlusA -> result
        }
    -> MinNat minimum
    -> result
equalOrGreaterThan tried minimum cases =
    \atLeast ->
        if toInt atLeast >= NNat.toInt tried then
            .equalOrGreater cases (toInt atLeast |> NatFrom)

        else
            .less cases (toInt atLeast |> Internal.InNat)


equalOrLessThan :
    NNat (N tried Is (Difference a To triedPlusA))
    -> NNat (N minimum Is (Difference triedToMinimum To tried))
    ->
        { equalOrLess : InNat minimum triedPlusA -> result
        , greater : MinNat (N1NatPlus tried) -> result
        }
    -> MinNat minimum
    -> result
equalOrLessThan tried minimum cases =
    \atLeast ->
        if toInt atLeast <= NNat.toInt tried then
            .equalOrLess cases (toInt atLeast |> Internal.InNat)

        else
            .greater cases (toInt atLeast |> NatFrom)
