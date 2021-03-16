module InNatTests exposing (suite)

{-| Especially type tests.
-}

import Expect
import InNat exposing (InNat)
import MinNat exposing (MinNat)
import MinNatTests
import N.Nat.Type exposing (..)
import N.Type exposing (..)
import NNat exposing (NNat)
import Nats exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "nat-in-range"
        [ describe "recursive"
            [ test "ultraSafeFactorial"
                (\() ->
                    ultraSafeFactorial (nat4 |> InNat.n)
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


natFactorial : MinNat Nat0 -> MinNat Nat1
natFactorial =
    MinNat.isAtLeast nat1
        { min = nat0 }
        { less = \_ -> MinNat.n nat1
        , equalOrGreater =
            \oneOrGreater ->
                oneOrGreater
                    |> MinNat.mul
                        (natFactorial
                            (oneOrGreater |> MinNat.subN nat1)
                        )
        }


factorial : MinNat minimum -> MinNat Nat1
factorial =
    MinNat.lowerMin nat0 >> natFactorial


ultraSafeFactorial : InNat minimum Nat18 -> MinNat Nat1
ultraSafeFactorial =
    InNat.dropMax >> MinNatTests.factorial



--


testAdd : InNat Nat4 (Nat22Plus a)
testAdd =
    InNat.intInRange nat3 nat10 7
        |> InNat.add (InNat.intInRange nat1 nat12 9) nat1 nat12


testAddN : InNat Nat15 (Nat19Plus a)
testAddN =
    InNat.intInRange nat6 nat10 7
        |> InNat.addN ( nat9, nat9 )


testSub : InNat Nat1 (Nat9Plus a)
testSub =
    InNat.intInRange nat6 nat10 7
        |> InNat.sub (InNat.intInRange nat1 nat5 4) nat1 nat5


testSubN : InNat Nat7 (Nat11Plus a)
testSubN =
    InNat.intInRange nat16 nat20 17
        |> InNat.subN ( nat9, nat9 )


testLowerMin : List (InNat Nat3 (Nat4Plus a))
testLowerMin =
    [ InNat.n nat3, InNat.n nat4 |> InNat.lowerMin nat3 ]
