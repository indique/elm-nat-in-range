module MinNatTests exposing (factorial, suite)

{-| Especially type tests.
-}

import Expect
import InNat exposing (InNat)
import MinNat exposing (MinNat)
import N.Nat.Type exposing (..)
import N.Type exposing (..)
import NNat exposing (NNat)
import NNats exposing (..)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "MinNat"
        [ test "factorial"
            (\() ->
                factorial (nat4 |> MinNat.n)
                    |> MinNat.toInt
                    |> Expect.equal 24
            )
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


listLength : List a -> MinNat Nat0
listLength =
    List.foldl
        (\_ -> MinNat.addN nat1 >> MinNat.lowerMin nat0)
        (MinNat.n nat0)



-- type tests


testAdd : MinNat Nat4
testAdd =
    MinNat.intAtLeast nat3 7
        |> MinNat.add (MinNat.intAtLeast nat1 9) nat1


testAddN : MinNat Nat15
testAddN =
    MinNat.intAtLeast nat6 7
        |> MinNat.addN nat9


testSubIn : MinNat Nat1
testSubIn =
    MinNat.intAtLeast nat6 7
        |> MinNat.subIn (InNat.intInRange nat1 nat5 4) nat5


testSubN : MinNat Nat7
testSubN =
    MinNat.intAtLeast nat16 17
        |> MinNat.subN nat9


testLowerMin : List (MinNat Nat3)
testLowerMin =
    [ MinNat.n nat3
    , MinNat.n nat4 |> MinNat.lowerMin nat3
    ]
