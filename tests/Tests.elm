module Tests exposing (suite)

import Expect
import N.Nat.Type exposing (..)
import N.Type exposing (..)
import NNat
import Test exposing (Test, describe, test)
import Try.InNat as InNat exposing (InNat)
import Try.MinNat as MinNat exposing (MinNat)


suite : Test
suite =
    describe "nat-in-range"
        [ describe "factorial versions"
            [ test "factorial"
                (\() ->
                    factorial (NNat.n4 |> MinNat.fromN)
                        |> MinNat.toInt
                        |> Expect.equal 24
                )
            , test "ultraSafeFactorial"
                (\() ->
                    ultraSafeFactorial (NNat.n4 |> InNat.fromN)
                        |> MinNat.toInt
                        |> Expect.equal 24
                )
            ]
        ]



--recurses idefinitely for negative integers


intFactorial : Int -> Int
intFactorial x =
    if x == 0 then
        1

    else
        x * intFactorial (x - 1)


factorial : MinNat minimum -> MinNat N1Nat
factorial =
    MinNat.isMin NNat.n0
        >> MinNat.equalOrLessThan NNat.n0
            NNat.n0
            { equalOrLess = \_ -> MinNat.fromN NNat.n1
            , greater =
                \oneOrMore ->
                    oneOrMore
                        |> MinNat.mul
                            (factorial
                                (oneOrMore |> MinNat.subN NNat.n1)
                            )
            }


ultraSafeFactorial : InNat minimum N18Nat -> MinNat N1Nat
ultraSafeFactorial =
    MinNat.fromIn >> factorial


testAdd : InNat (N1NatPlus N3Nat) (N22NatPlus a)
testAdd =
    InNat.fromBetween NNat.n3 NNat.n10 7
        |> InNat.add (InNat.fromBetween NNat.n1 NNat.n12 9) NNat.n1 NNat.n12


testAddNNat : InNat N15Nat (N19NatPlus a)
testAddNNat =
    InNat.fromBetween NNat.n6 NNat.n10 7
        |> InNat.addN ( NNat.n9, NNat.n9 )


testSub : InNat N1Nat (N9NatPlus a)
testSub =
    InNat.fromBetween NNat.n6 NNat.n10 7
        |> InNat.sub (InNat.fromBetween NNat.n1 NNat.n5 4) NNat.n1 NNat.n5


testSubNNat : InNat N7Nat (N11NatPlus a)
testSubNNat =
    InNat.fromBetween NNat.n16 NNat.n20 17
        |> InNat.subN ( NNat.n9, NNat.n9 )
