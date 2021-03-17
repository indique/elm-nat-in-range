## elm-nat-in-range

Current attempts of represening natural numbers `>= 0` only have quite lose promises.

- If you know that your `Nat` is >= 3, why can't you subtract 1 or 2 or 3?

`MinNat` & `InNat` ensure that a `Nat` is within a minimum (& maximum)!

- `MinNat` only knows what a nat at least is:  `⨯[✓✓✓✓✓✓✓...`
- `InNat` knows the range a nat is in:  `⨯⨯[✓✓✓]⨯⨯...`

A strict `Nat` type, where the _actual value_ is present in the type is [`NNat`][elm-n-nat].

Setup

```noformatingplease
elm install indique/elm-n-type
elm install indique/elm-n-nat-type
elm install indique/elm-n-nat
elm install indique/elm-nat-in-range
```

```elm
import MinNat exposing (MinNat)
import InNat exposing (InNat)
import NNat exposing (NNat)
import NNats exposing (..) --nat0 to nat192

import N.Type exposing (..)
import N.Nat.Type exposing (..)
    --Nat0 to Nat192 & Nat0Plus to Nat192Plus
```

## 2 examples


### `color`

```elm
rgb : Float -> Float -> Float -> Color
```

This is common, but _the one implementing_ the function has to handle the case where a value is not between 0 and 1.

```elm
rgbPer100 :
    InNat redMin Nat100
    -> InNat greenMin Nat100
    -> InNat blueMin Nat100
    -> Color
```
Here, _the one using_ this function must make sure to you that the numbers are actually between 0 and 100.

They can prove it by

- clamping
  ```elm
  grey float =
      let
          from0To100 =
              (float * 100) |> floor
                  |> InNat.intInRange nat0 nat100 --easy
      in
      rgbPer100 from0To100 from0To100 from0To100
  ```
- already knowing
  ```elm
  nat100
  -- of type NNat Nat100
  -- → We know the EXACT value
      |> InNat.n
      -- → it must also be in range 100 to 100 + ...
  red =
    rgbPer100 (nat100 |> InNat.n)
        (nat0 |> InNat.n) (nat0 |> InNat.n)
        -- 👍
  ```
- checking
  ```elm
  isUserIntANat : Int -> Maybe (MinNat Nat0)
  isUserIntANat =
      MinNat.isAtLeast nat0
          { equalOrGreater = Just
          , less = \_-> Nothing
          }
  ```

- There are more ways, but this is enough to understand the idea 🙂.

&emsp;



### `factorial`

```elm
intFactorial : Int -> Int
intFactorial x =
    if x == 0 then
        1

    else
        x * intFactorial (x - 1)
```

This forms an infinite loop if we call `intFactorial -1`...

Let's disallow negative numbers here!

```elm
natFactorial : MinNat Nat0 -> MinNat Nat1
```
Says: for every natural number `n >= 0`, `n! >= 1`.
```elm
natFactorial =
    MinNat.isAtLeast nat1
        { min = nat0 } -- the minimum of the x
        { less = \_ -> MinNat.n nat1 -- x < 1 ? → then 1
        , equalOrGreater = -- x >= 1 ?
            \oneOrGreater -> --we now know it is a MinNat Nat1
                oneOrGreater
                    |> MinNat.mul
                        (natFactorial
                            (oneOrGreater |> MinNat.subN nat1)
                            -- so we can safely subtract 1 👍
                        )
        }
```
As the minimum is allowed to be anything `>= 0`:
```
factorial : MinNat min -> MinNat Nat1
factorial =
    MinNat.lowerMin nat0 >> natFactorial
```

→ `factorial (MinNat.n nat4) |> MinNat.toInt --> 24`

→ There is no way to put a negative in.

We can do even better.
We know that `!19` is already bigger than the maximum safe `Int` `2^53 - 1`.

```elm
safeFactorial : InNat min N18Nat -> MinNat Nat1
safeFactorial =
    MinNat.fromIn >> factorial
```


## tips

- keep as much type information as possible and drop it only where you don't need it.
    ```elm
    squaresTo10 =
        InNat.range nat0 nat10
            --nats from 0 to 10
            |> List.map
                (MinNat.dropMax
                    --we can no longer compute the maximum
                    >> MinNat.toPower 2
                    --we can't compute the exact minimum
                    --but we know its at least Nat0
                )
    ```
- keep your function annotations as general as possible
    
    Instead of accepting only values where you know the values exact
  ```elm
  rgb : NNat (N red Is (Difference inverseRed To Nat100)) --...
  ```
    accept values that are somewhere in a range.
  ```elm
  rgb : InNat redMin Nat100 --...
  ```

Take a look at [`elm-n-array`][elm-n-array] to see `InNat` in action!

[elm-n-nat]: https://package.elm-lang.org/packages/indique/elm-n-nat/latest/
[elm-n-array]: https://package.elm-lang.org/packages/indique/elm-n-array/latest/
