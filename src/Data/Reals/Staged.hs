{- | A staged computation is a sequence of approximations which represents its limit. Specifically,
  we think of modeling a topological space as a subspace of a continuous domain with a countable
  base, for theoretical background see the paper http://dx.doi.org/10.1016/j.apal.2008.09.025 (also
  available at http://math.andrej.com/)

  Suppose we would like to represent a space X, where X is some sort of a complete space (either
  metrically or domain-theoretically complete). We think of the points of X as limits of sequences
  of approximations. For example, a real number may be thought of as a sequence of intervals which
  all contain x and whose widths converge to 0. In the general case the approximations can be
  abstract entities that need not correspond to intervals (although each approximation naturally
  corresponds to the subset of those points that it approximates.) The approximations are naturally
  order by how well they approximate the values (for intervals this is reverse inclusion) and they form
  a /base/ for a continuous domain, see the cited paper above.

  If @b@ ("b" for the "base") is the datatype which represents the approximations, then the elements
  of the space could be represented by the datatype @Int -> b@. However, in practice we need to
  control the direction of approximation: a real number may be rounded up or down, a set may be
  approximated from inside or from outside, etc. Thus we include rounding information in the
  sequence, so that an element of the space is represented by the datatype @'Stage' -> b@ where
  'Stage' carries rounding information and the index.

  We empahsize that the rounding mode is /not/ that of floating point arithmetic. Rather, it tells
  us whether the exact results should be approximated from below or above in the natural order of
  approximations. Typically, computations based on domain-theoretic models always approximate from
  below, but there are uses for over-approximations as well, for example when we estimate the truth
  value of a quantifier. Therefore we allow approximating sequences which approach their limit from
  above in the domai-theoretic order.

  It is cumbersome to work with the datatype @Stage -> b@ explicitly because we need to manually pass
  around the @Stage@ parameter. Haskell comes in handy here, as we define a monad which is very much
  like the @Reader@ monad of Haskell.
-}

module Data.Reals.Staged (
              prec,
              Stage (..),
              RoundingMode (..),
              Completion (..),
              Staged (..),
              MStaged (..)
) where

import Data.Approximate.ApproximateField
import Control.Applicative
--import Debug.Trace

{- | The 'Completion' class represents a completion operation. An instance @m@ of class 'Completion'
   is a type constructor which takes a type @b@ representing the base, i.e., the approximations, and
   gives the type @m b@ of the completion of @b@. For example, if @b@ is the datatype of dyadic
   intervals, then @m b@ would be the interval domain.

   Each element of the completion @m b@ may be converted to a sequence of approximations with the
   "approximate" function. Conversely, a sequence of approximations may be converted to the element
   with the "limit" function.
-}

class Applicative m => Completion m where
    getStage :: m Stage -- ^ get the current stage
    getRounding :: m RoundingMode -- ^ get the current rounding
    getPrec :: m Int -- ^ get the current precision
--    getPrec2 :: m t -> Int

    approximate :: m t -> Stage -> t -- ^ approximate by a chain (from above or from below, depending on rounding mode)
    limit :: (Stage -> t) -> m t -- ^ the element represented by a given chain

    embed :: t -> m t -- ^ a synonym for @return@
    embed = pure

    -- | lift a map from approximations to points
    lift1 :: (Stage -> t -> u) -> m t -> m u
    lift1 f = liftA2 f getStage

    -- | lift a map of two arguments from approximations to points.
    lift2 :: (Stage -> t -> u -> v) -> m t -> m u -> m v
    lift2 f = liftA3 f getStage

-- | If @t@ is the type of approximations then, @Staged t@ is the type of the points of the space,
-- represented as sequences of approximations.
newtype Staged t = Staged { approx :: Stage -> t }

-- | The functor structure of 'Staged' is the same as that of the @Reader@ monad.
instance Functor Staged where
    fmap f x = Staged $ \s -> f (approx x s)

instance Applicative Staged where
    pure a    = Staged $ const a
    (<*>) f x = Staged $ \s -> approx f s (approx x s)

--    (<*>) f x = Staged $ \s -> approx f s (approx x s)

-- | 'Staged' is an instance of a completion.
instance Completion Staged where
    getStage = Staged id
    getRounding = Staged rounding
    getPrec = Staged precision
    approximate st s = {-traceShow ("approximate", s)-} (approx st s)
    limit = Staged

newtype MStaged t = MStaged { mapprox :: Stage -> (t, MStaged t) }

-- | The functor structure of 'Staged' is the same as that of the @Reader@ monad.
instance Functor MStaged where
    fmap f x = MStaged $ approx'
      where approx' s = (f approx1, fmap f approx2 )
             where
                   (approx1, approx2) = mapprox x s

instance Applicative MStaged where
    pure a    = MStaged $ \s -> (a, pure a)
    (<*>) f x = MStaged $ approx'
                           where approx' s = (b1 a1, b2 <*> a2)
                                                            where (a1, a2) = mapprox x s
                                                                  (b1, b2) = mapprox f s

instance Completion MStaged where
    getStage = MStaged $ \s -> (s, getStage)
    getRounding = MStaged $ \s -> (rounding s, getRounding)
    getPrec = MStaged $ \s -> (precision s, getPrec)
    approximate st s = {-traceShow ("approximate", s)-} fst (mapprox st s)
    limit f = MStaged $ \s -> (f s, limit f)

{-
class Approximation a where
   width :: a -> Int

class ContFunctor f  where
    fmapc        :: Approximation (f b) => (a -> b) -> f a -> f b

    -- | Replace all locations in the input with the same value.
    -- The default definition is @'fmap' . 'const'@, but this may be
    -- overridden with a more efficient version.
    (<$)        :: Approximation (f a) => a -> f b -> f a
    (<$)        =  fmapc . const

class ContFunctor f => ContApplicative f where
    -- | Lift a value.
    purec :: Approximation (f a) => a -> f a

    -- | Sequential application.
    (<*>) :: Approximation (f b) => f (a -> b) -> f a -> f b

    -- | Sequence actions, discarding the value of the first argument.
    (*>) :: f a -> f b -> f b
    (*>) = liftA2c (const id)

    -- | Sequence actions, discarding the value of the second argument.
    (<*) :: f a -> f b -> f a
    (<*) = liftA2c const
-}
