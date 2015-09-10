{- | This module contains specification of approximate field.
-}

module ApproximateField (
       RoundingMode (..),
       Stage (..),
       anti,
       precDown,
       precUp,
       prec,
       ApproximateField (..),
       Midpoint (..)
) where

-- | The rounding mode tells us whether we should under- or over-approximate the exact result.
data RoundingMode = RoundUp | RoundDown
                  deriving (Eq, Show)

-- | A stage of computation tells us how hard we should try to compute the result. The 'stage' component
-- is a measure of precisions. As it goes to infinity, the approximation should converge to the exact
-- value (in the sense of Scott topology on the underlying domain model).
data Stage = Stage { precision :: Int, rounding :: RoundingMode }
             deriving Show

-- | 'anti' reverses the rounding mode
anti :: Stage -> Stage
anti s = Stage {precision = precision s, rounding = case rounding s of { RoundUp -> RoundDown ; RoundDown -> RoundUp}}

-- | @precDown k@ sets precision to @k@ and the rounding mode to 'RoundDown'
precDown :: Int -> Stage
precDown k = Stage {precision = k, rounding = RoundDown}

-- | @precUp k@ sets precision to @k@ and the rounding mode to 'RoundUp'
precUp :: Int -> Stage
precUp k = Stage {precision = k, rounding = RoundUp}

-- | @prec r k@ is the stage with given rounding @r@ and precision @k@
prec :: RoundingMode -> Int -> Stage
prec r k = Stage {precision = k, rounding = r}




{- | An approximate field is a structure in which we can perform approximate
arithmetical operations. The typical example is the ring of dyadic rational
numbers: division of dyadic rationals is only approximate, and even though the
other operations (+, -, *) can be peformed exactly, it is too expensive and
unecessary to do so in an interval computation. Therefore, we want approximate
versions of all operations.

The approximate operations take a 'Stage' argument which tells whether the
result of the operation should be rounded up or down, in the sense of the
linear ordering of the structure, and how precise the result should be.

(Missing explanation of what exactly an approximate field is supposed to be.)
-}
class (Show q, Ord q) => ApproximateField q where
--  normalize :: Stage -> q -> q
--  size :: q -> Int -- ^ the size of the number (memory usage)
--  log2 :: q -> Int -- ^ @log2 q@ is a number @k@ such that @2^k <= abs q <= 2^(k+1)@.

  zero :: q
  positive_inf :: q
  negative_inf :: q

--  toFloat :: q -> Double

  -- approximate operations
  app_add :: Stage -> q -> q -> q
  app_sub :: Stage -> q -> q -> q
  app_mul :: Stage -> q -> q -> q
  app_inv :: Stage -> q -> q
  app_div :: Stage -> q -> q -> q
  app_negate :: Stage -> q -> q
--  app_abs :: Stage -> q -> q
--  app_signum :: Stage -> q -> q
  app_fromInteger :: Stage -> Integer -> q
  app_fromRational :: Stage -> Rational -> q
--  app_shift :: Stage -> q -> Int -> q -- ^ shift by a power of 2

class Midpoint q where
  midpoint :: q -> q -> q -- ^ exact midpoint
