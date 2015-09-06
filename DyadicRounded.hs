{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- | This module contains a faster implementation of dyadic rationals using
   a fast arbitrary-precision floating-point library MPFR via haskel module Data.Rounded
-}

module DyadicRounded (
  exact
) where

import Prelude hiding (isNaN,isInfinite, div)
--import Data.Number.MPFR hiding (less)
import Data.Number.Rounded
import Staged hiding (prec)
import ApproximateField
import Interval
import Reals

{- | Dyadics with normalization and rounding form an "approximate"
  field in which operations can be performed up to a given precision.

  We take the easy route: first we perform an exact operation then we
  normalize the result. A better implementation would directly compute
  the approximation, but it's probably not worth doing this with
  Dyadics. If you want speed, use hmpfr, see
  <http://hackage.haskell.org/package/hmpfr>.
-}

staged2mpfrRounding RoundUp = Up
staged2mpfrRounding RoundDown = Down

rnd = staged2mpfrRounding . rounding

prec :: Stage -> Precision
prec a = fromInteger (max 2 (toInteger (precision a)))

instance Num Rounded where
  fromInteger = fromIntegerA Near 53

instance ApproximateField Rounded where
{-  normalize s a | isNaN a = case rounding s of
                      RoundDown -> negative_inf
                      RoundUp -> positive_inf
  normalize s a = set (rnd s) (prec s) a

  size = fromInteger . toInteger . Data.Number.Rounded.getPrec
-}

--  log2 NaN = error "log2 of NaN"
--  log2 PositiveInfinity = error "log2 of +inf"
--  log2 NegativeInfinity = error "log2 of -inf"
--  log2 Dyadic{mant=m, expo=e} = e + ilogb 2 m

  zero = Data.Number.Rounded.zero
  positive_inf = posInf
  negative_inf = negInf


  app_add s = add (rnd s) (prec s)
  app_sub s = sub (rnd s) (prec s)
  app_mul s = mul (rnd s) (prec s)
  app_negate s = neg (rnd s) (prec s)
--  app_abs s = absD (rnd s) (prec s)
{-  app_signum s a = case sgn a of
  	Nothing -> error "sign of NaN"
	Just i -> fromInt (rnd s) (prec s) i
-}
  app_fromInteger s = fromIntegerA (rnd s) (prec s)
  app_fromRational s = fromRationalA (rnd s) (prec s)

  app_inv s a = div (rnd s) (prec s) (fromIntegerA Near 2 1) a
{-  app_inv s NaN = normalize s NaN
  app_inv s PositiveInfinity = zero
  app_inv s NegativeInfinity = zero
  app_inv s Dyadic{mant=m, expo=e} =
    let d = prec s
        b = ilogb 2 m
        r = case rounding s of
              RoundDown -> 0
              RoundUp -> 1
    in if signum m == 0
       then normalize s NaN
       else Dyadic {mant = r + (shiftL 1 (d + b)) `div` m, expo = -(b + d + e)}
-}
  app_div s = div (rnd s) (prec s)
{-  app_div s Dyadic{mant=m1,expo=e1} Dyadic{mant=m2,expo=e2} =
      let e = prec s
          r = case rounding s of
                RoundDown -> 0
                RoundUp -> 1
      in if signum m2 == 0
      then normalize s NaN
      else Dyadic {mant = r + (shiftL 1 e * m1) `div` m2, expo = e1 - e2 - e}
  app_div s _ _ = normalize s NaN -- can we do better than this in other cases?
-}
--  app_shift s = mul2i (rnd s) (prec s)
{-  app_shift s NaN k = normalize s NaN
  app_shift s PositiveInfinity k = PositiveInfinity
  app_shift s NegativeInfinity k = NegativeInfinity
  app_shift s Dyadic {mant=m, expo=e} k = normalize s (Dyadic {mant = m, expo = e + k})
-}

instance Midpoint Rounded where
--  toFloat = toDouble Near

--  midpoint a b | isNaN a = a
--  midpoint a b | isNaN b = b
--  midpoint a b | isInfinite a && isInfinite b && a == b = a
--  midpoint a b | isInfinite a && isInfinite b = Data.Number.Rounded.zero
  midpoint a b = mul2i Near p (add Near p a b) (-1)
           where p = 1 + maxPrec a b
                 maxPrec a b = max (Data.Number.Rounded.getPrec a) (Data.Number.Rounded.getPrec b)

{-  midpoint NaN _ = NaN
  midpoint _ NaN = NaN
  midpoint NegativeInfinity NegativeInfinity = NegativeInfinity
  midpoint NegativeInfinity PositiveInfinity = zero
  midpoint NegativeInfinity Dyadic{mant=m, expo=e} = Dyadic {mant = -1 - abs m, expo= 2 * max 1 e}
  midpoint PositiveInfinity NegativeInfinity = zero
  midpoint PositiveInfinity PositiveInfinity = PositiveInfinity
  midpoint PositiveInfinity Dyadic{mant=m, expo=e} = Dyadic {mant = 1 + abs m, expo= 2 * max 1 e}
  midpoint Dyadic{mant=m,expo=e} NegativeInfinity = Dyadic {mant = -1 - abs m, expo= 2 * max 1 e}
  midpoint Dyadic{mant=m,expo=e} PositiveInfinity = Dyadic {mant = 1 + abs m, expo= 2 * max 1 e}
  midpoint Dyadic{mant=m1,expo=e1} Dyadic{mant=m2,expo=e2} = Dyadic {mant = m3, expo = e3 - 1}
    where m3 = if e1 < e2 then m1 + shiftL m2 (e2 - e1) else shiftL m1 (e1 - e2) + m2
          e3 = min e1 e2
-}

-- | This is a convenience function which allows us to write @exact 1.3@ as a
-- conversion from floating points to real numbers. There probably is a better way of
-- doing this.
exact :: RealNum Rounded -> RealNum Rounded
exact x = x
