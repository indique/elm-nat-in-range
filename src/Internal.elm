module Internal exposing (InNat(..), MinNat(..), inNatToInt, minNatToInt)

{-| Both `InNat` & `MinNat` can use these.
-}


type InNat minimum maximum
    = InNat Int


type MinNat min
    = MinNat Int


minNatToInt : MinNat min -> Int
minNatToInt =
    \(MinNat int) -> int


inNatToInt : InNat min max -> Int
inNatToInt =
    \(InNat int) -> int
