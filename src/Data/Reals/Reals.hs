{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

{- | We implement real numbers as the completion of dyadic intervals. The whole construction is
   parametrized by an approximate field, an example of which is "Dyadic".
-}

module Data.Reals.Reals (
             RealNum, approximate, ClosedInterval (..), forall
) where

import Data.Approximate.ApproximateField
import Debug.Trace
import Data.Ratio
import Data.Approximate.Interval
import Data.Reals.Space
import Data.Reals.Staged
import Data.Approximate.Floating.MPFR
import Numeric.AD.Internal.Forward
import Numeric.AD.Internal.Type

-- | A real number is implemented as a staged dyadic interval @'Interval' q@ where @q@ is the
-- underlying approximate field (in practice these are dyadic rationals). @'RealNumQ' q@ can be used
-- to represent not only real numbers but also the elements of the interval domain, including the
-- back-to-front intervals.

type RealNumQ q = StagedWithList (Interval q)
type RealNum = RealNumQ Rounded

-- | We implement a very simple show instance for reals which computes the 20th approximation
-- and shows it as an interval, together with a floating point approximation.
instance ApproximateField q => Show (RealNumQ q) where
   show x = let a = approximate x 20 
                i = Data.Reals.Staged.lower $ a
                j = Data.Reals.Staged.upper $ a
            in show (i,j)

-- | Linear order on real numbers
instance Ord (Interval q) => LinearOrder (RealNumQ q) Sigma where
    less = lift2 (\s -> (<))

-- | The Hausdorff property
instance Ord (Interval q) => Hausdorff (RealNumQ q) where
     x `apart` y = (x `less` y) `sor` (y `less` x)

-- | It is a bad idea to use Haskell-style inequality @/=@ on reals because it either returns @True@
-- or it diverges. Similarly, using Haskell equality @==@ is bad. Nevertheless, we define @==@ and @/=@
-- because Haskell wants them for numeric types.
instance Ord (Interval q) => Eq (RealNumQ q) where
    x /= y = force $ x `apart` y

-- | Real numbers are an ordered type in the sense of Haskells 'Ord', although a comparison never
-- returns @EQ@ (instead it diverges). This is a fact of life, comparison of reals is not decidable.
instance Ord (Interval q) => Ord (RealNumQ q) where
  compare x y = if force (x `less` y) then LT else GT

-- | The ring structure fo the reals.
instance (DyadicField q, ApproximateField (Interval q)) => Num (RealNumQ q) where
    (+) = lift2 appAdd
    (-) = lift2 appSub
    (*) = lift2 appMul

    abs = lift1 appAbs

    signum _ = error "could diverge"
    {- signum x = do i <- x
                      s <- getStage
                      return Interval { lower = app_signum s (lower i),
                                          upper = app_signum (anti s) (upper i) --}

    fromInteger k = limit $ \s -> Approximation i i
                      where i = appFromInteger k

-- | Division and reciprocals.
instance (DyadicField q, ApproximateField (Interval q)) => Fractional (RealNumQ q) where
    (/) = lift2 appDiv

    recip = lift1 appInv

    fromRational r = limit $ \p -> Approximation (appFromRational (precDown p) r) (appFromRational (precUp p) r)
                               -- ({-traceShow ("fr",r, s)-} appFromRational s r )--, fromRational r)

-- | The value @ClosedInterval(a,b)@ represents the closed interval [a,b] as a subspace of the reals.
newtype ClosedInterval q = ClosedInterval (q, q)

{-
Comparison of using (prec, rounding) -> approximation vs. prec -> (lower, upper):
*Data.Reals.Reals> forall int $ \x -> (x * (1 - x)) `less` (0.252 :: RealNum )
True
(0.14 secs, 39248160 bytes)
(0.07 secs, 22814536 bytes)
*Data.Reals.Reals> forall int $ \x -> (x * (1 - x)) `less` (0.251 :: RealNum )
True
(0.24 secs, 77898296 bytes)
(0.07 secs, 22766448 bytes)
*Data.Reals.Reals> forall int $ \x -> (x * (1 - x)) `less` (0.2501 :: RealNum )
True
(2.40 secs, 1506857904 bytes)
(0.20 secs, 68443912 bytes)
-}

-- | Compactness of the closed interval
instance (DyadicField q) => Compact (ClosedInterval q) (RealNumQ q) where
   forall (ClosedInterval(a,b)) p =
     limit (\n ->
       let test_interval u v = Approximation (Interval u v) (let w = midpoint u v in Interval w w)
           x = limit $ let t = test_interval a b in \n -> t
           ap = approximate (p x) 0
           p2 x = limit $ \k -> approximate (p x) (k+1)
       in case (Data.Reals.Staged.lower ap, Data.Reals.Staged.upper ap) of
            (True, _) -> {-traceShow ("holds", a,b)$-} Approximation True True
            (_, False) -> {-traceShow ("proof", midpoint a b) $-} Approximation False False
            otherwise -> if n <= 0 then Approximation False True
                                   else let c = midpoint a b in approximate (sand (forall (ClosedInterval(a,c)) p2) (forall (ClosedInterval(c,b)) p2)) (n-1)
     )
     --          if (k >= n) then Approximation False (Data.Reals.Staged.upper $ sweep lst)

{-     
instance (DyadicField q, LinearOrder t l) => Compact2 (ClosedInterval q) t l where
   forall2 i@(ClosedInterval(a,b)) p =
     limit (\n ->
       let test_interval u v = Approximation (Interval u v) (let w = midpoint u v in Interval w w)
     --      zblj = estimate p i
           sweep [] = Approximation True True
           sweep ((k,a,b):lst) = let x = limit $ let t = test_interval a b in \n -> t
                                     ap = approximate (p x) k
                                  in case (Data.Reals.Staged.lower ap, Data.Reals.Staged.upper ap) of
                                      (True, _) -> traceShow ("holds", n,a,b) $ sweep lst
                                      (_, False) -> traceShow ("proof", midpoint a b) $ Approximation False False
                                      otherwise -> if (k >= n) then Approximation False (Data.Reals.Staged.upper $ sweep lst)
                                                               else (let c = midpoint a b in sweep ((k+1,a,c) : (k+1,c,b) : lst))
       in sweep [(0,a,b)]
     )
  -}   
-- | Overtness of reals on closed interval [a,b]
instance (DyadicField q) => Overt (ClosedInterval q) (RealNumQ q) where
    exists (ClosedInterval (a,b)) p =
      limit (\n ->
        let test_interval u v = Approximation (let w = midpoint u v in Interval w w) (Interval v u)
            sweep [] = Approximation False False
            sweep ((k,a,b):lst) = let x = limit $ let t = test_interval a b in \n -> t
                                      ap = approximate (p x) k
                                    in case (Data.Reals.Staged.lower ap, Data.Reals.Staged.upper ap) of
                                      (True, _)  -> traceShow ("witness ", midpoint a b) $ Approximation True True
                                      (_, False) -> traceShow ("not in ",a,b) $ sweep lst
                                      otherwise-> if (k >= n) then Approximation (Data.Reals.Staged.lower $ sweep lst) True
                                                              else (let c = midpoint a b in sweep ((k+1,a,c) : (k+1,c,b) : lst))
       in sweep [(0,a,b)]
     )

data Estimate a
  = Estimate a a deriving (Show)

instance Fractional a => LinearOrder (Forward a) (Estimate a) where
   less a b = Estimate (primal a - primal b) (tangent a - tangent b)

--estimate :: (LinearOrder t s, Fractional t) => (t -> s) -> ClosedInterval Rounded -> RealNum
estimate f (ClosedInterval (x,y)) = 
    limit (\n ->
      let test_interval u v = (limit $ \n -> (Approximation (Interval u v) (let w = midpoint u v in Interval w w))) :: RealNum
          i = test_interval x y
          xm = midpoint x y
          Estimate valueapp derivativeapp = (apply f i) :: Estimate RealNum
          Interval lf uf = Data.Reals.Staged.upper $ approximate valueapp n
          Interval ld ud = Data.Reals.Staged.lower $ approximate derivativeapp n
          subU = appSub (precUp n)
          divU = appDiv (precUp n)
          subD = appSub (precDown n)
          divD = appDiv (precDown n)          
          leD = if ld == zero then posInf else xm `subD` (lf `divD` ld) :: Rounded
          leU = if ld == zero then negInf else xm `subU` (lf `divU` ld) :: Rounded
          ueD = if ud == zero then posInf else xm `subD` (lf `divD` ud) :: Rounded
          ueU = if ud == zero then negInf else xm `subU` (lf `divU` ud) :: Rounded
          ugD = xm `subD` (uf `divD` ud) :: Rounded
          ugU = xm `subU` (uf `divU` ud) :: Rounded
          lgD = xm `subD` (uf `divD` ld) :: Rounded
          lgU = xm `subU` (uf `divU` ld) :: Rounded
          (lb,ub) = case (lf < zero, zero < ld, ud < zero) of
                      (True,  True, _)    -> (leU,    posInf)
                      (True,  _,    True) -> (negInf, ueD)
                      (True,  _,    _)    -> (posInf, negInf)
                      (False, True, _)    -> (ueU,    posInf)
                      (False, _,    True) -> (negInf, leD)
                      (False, _,    _)    -> (ueU,    leD)
          (lc,uc) = case (uf < zero, zero < ld, ud < zero) of
                      (True,  True, _)    -> (negInf, ugD)
                      (True,  _,    True) -> (lgU, posInf)
                      (True,  _,    _)    -> (lgU, ugD)
                      (False, True, _)    -> (negInf, lgD)
                      (False, _,    True) -> (ugU, posInf)
                      (False, _,    _)    -> (posInf, negInf)
--          z = x 
      in traceShow("imed", xm,lf,ld,ud,leD,ueD) $ Approximation (Interval lc uc) (Interval lb ub) -- (max lb x) (min ub y)) (Interval ld ud)
    ) :: RealNum
{-

-- | Reals form a complete space, which means that every Cauchy sequence of reals has
-- a limit. In the implementation this is manifested by the existence of an operator
-- which computes the limit of a Cauchy sequence. The error bounds for the sequence are
-- given explicitly.
lim :: (Int -> (RealNumQ q, q)) -> RealNumQ q
lim x =
    error "Limits of Cauchy sequences are not implemented"
    {- there are several ways to implement 'lim', see the end of Section 7 of

       http://math.andrej.com/2008/01/31/a-constructive-theory-of-domains-suitable-for-implementation/

    -}



-- | Reals form an Archimedean field. Topologically speaking, this means that the
-- underlying approximate field @q@ is dense in the reals. Computationally this means
-- that we can compute arbitrarily good @q@-approximations to real numbers. The
-- function 'approxTo x k' computes an approximation @a@ of type @q@ which is within
-- @2^-k@ of @x@.
approxTo :: RealNumQ q -> Int -> q
approxTo x k =
    error "approxTo not implemented"
    {- the simplest way to implement approxTo is to keep calling
       @approximate x (prec RoundDown n)@ until @n@ is so large that the returned
       interval has width at most @2^(-n-1)@. The center of the
       interval is the approximation we seek.

       There are several possibilities for optimization. Here we describe one. Let
       @a_n@ be the midpoint of the interval @approximate x (prec RoundDown n)@ and
       let @r_n@ be its width. We are looking for @n@ such that @r_n < 2^{ -n-1}@.
       One heuristic is to assume that @r_n@'s form a geometric series. From this we
       can look at three terms of the sequence, say @r_10@, @r_20@, and @r_30@ and
       estimate an @n@ which should give @r_n < 2^{ -n-1}@. If the estimate fails,
       we try something else. The drawback is that we might end up over-estimating
       the needed precision @n@. -}
-}
