{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

{- | This module defines the interval domain, i.e., the space of
  intervals. Actually, what we define as a /base/ for such a domain
  because our intervals have rational endpoints (to be exact, the
  endpoints are elements of an approximate field). The actual interval
  domains is defined in the module "Reals".
-}

module Data.Approximate.Interval (
  Interval (..),
  width, split,
) where

import Data.Approximate.ApproximateField
--import Debug.Trace


{- | An interval is represented by a lower and upper endpoint. We do
  /not/ require that the lower endpoint be smaller or equal to the
  upper one. In other words, we allow the usual as well as
  back-to-front intervals. This is useful in certain kinds of
  computations.

  A possible optimization: represent an interval with its center point
  and radius, where the radius is not a precise number (has a small
  mantissa). This can save up to 50% of space, but it is not clear how
  to treat back-to-front intervals then. Presumably with negative
  radii, except I have never worked out how to implement interval
  multiplication then. -}

data Interval q = Interval { lower :: q, upper :: q }
  deriving Eq

instance (Show q, Eq q) => Show (Interval q) where
  show Interval{lower=a, upper=b} =
    if a == b
    then show a
    else "[" ++ show a ++ "," ++ show b ++ "]"


{- | We define the implementation of intervals in terms of ApproximateField. -}

instance Ord q => Ord (Interval q) where
  i < j = upper i < lower j
  i > j = j < i
  i <= j = upper i <= lower j


instance DyadicField q => ApproximateField (Interval q) where
  appAdd s a b = Interval { lower = appAdd s (lower a) (lower b),
                              upper = appAdd (anti s) (upper a) (upper b)}

  appSub s a b = Interval { lower = appSub s (lower a) (upper b),
                              upper = appSub (anti s) (upper a) (lower b)}

  appNeg s b = Interval { lower = appNeg s (upper b),
                              upper = appNeg (anti s) (lower b)}

  -- Kaucher multiplication
  appMul s Interval{lower=a,upper=b} Interval{lower=c,upper=d} =
    let negative q = q < zero
        lmul = appMul s
        umul = appMul (anti s)
    in {-traceShow s-} Interval { lower =
                                   case (negative a, negative b, negative c, negative d) of
                                     (False, False, True, _) -> lmul b c
                                     (False, False, False, _) -> lmul a c
                                     (True, True, _, True) -> lmul b d
                                     (True, True, _, False) -> lmul a d
                                     (True, _, False, False) -> lmul a d
                                     (False, _, False, False) -> lmul a c
                                     (_, False, True, True) -> lmul b c
                                     (_, True, True, True) -> lmul b d
                                     (True, False, True, False) -> min (lmul a d) (lmul b c)
                                     (False, True, False, True) -> max (lmul a c) (lmul b d)
                                     otherwise -> zero,
                                  upper =
                                   case (negative a, negative b, negative c, negative d) of
                                     (True, True, True, _) -> umul a c
                                     (True, True, False, _) -> umul b c
                                     (False, False, _, True) -> umul a d
                                     (False, False,_, False) -> umul b d
                                     (True, _, True, True) -> umul a c
                                     (False, _, True, True) -> umul a d
                                     (_, False, False, False) -> umul b d
                                     (_, True, False, False) -> umul b c
                                     (True, False, True, False) -> max (umul a c) (umul b d)
                                     (False, True, False, True) -> min (umul a d) (umul b c)
                                     otherwise -> zero
                                }

  appInv s Interval{lower=a, upper=b} =
    let sgn q = compare q zero
        linv = appInv s
        uinv = appInv (anti s)
    in Interval { lower = case (sgn a, sgn b) of
                             (LT, LT) -> linv b
                             (EQ, LT) -> linv b
                             (GT, GT) -> linv b
                             (GT, _) -> posInf
                             otherwise -> negInf,
                  upper = case (sgn a, sgn b) of
                             (LT, LT) -> uinv a
                             (GT, EQ) -> uinv a
                             (GT, GT) -> uinv a
                             (_, LT) -> negInf
                             otherwise -> posInf
                             }

  appDiv s a b = appMul s a (appInv s b)

  appAbs s a = Interval { lower = zero,
                          upper = let q = appNeg s (lower a)
                                      r = upper a
                                  in if q < r then r else q }
  appFromInteger k = Interval { lower = appFromInteger k,
                                  upper = appFromInteger k }

  --TODO
  appFromRational_ s r = (Interval { lower = appFromRational s r,
                                                       upper = appFromRational (anti s) r}, False)

width :: DyadicField q => Interval q -> Int
width Interval{lower=a, upper=b} = if zero == diff then maxBound else  (appGetExp diff) --- (appGetExp sum) + 1
    where diff = appSub (precUp 20) b a
          sum = appAdd (precUp 20) a b

split :: DyadicField q => Interval q -> (Interval q, Interval q)
split Interval{lower=a, upper=b} =
    let c = midpoint a b
    in (Interval {lower=a, upper=c}, Interval {lower=c, upper=b})
