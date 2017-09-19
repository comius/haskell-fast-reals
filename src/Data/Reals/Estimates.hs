{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

module Data.Reals.Estimates (
             forall, estimate, Estimate, forallEstimate
) where

import Data.Approximate.Floating.MPFR 
import Data.Reals.Staged
import Data.Reals.Reals
import Data.Approximate.ApproximateField
import Data.Approximate.Interval
import Data.Reals.Space
import Numeric.AD.Internal.Forward
import Numeric.AD.Internal.Type
import Data.NumInstances

type EstimateQ q = StagedWithFun ([Interval q])
type Estimate = EstimateQ Rounded

instance DyadicField q => Compact (ClosedInterval q) (Forward (RealNumQ q,RealNumQ q)) (EstimateQ q) where
   forall = forallEstimate

forallEstimate :: DyadicField q => ClosedInterval q -> (Forward (RealNumQ q, RealNumQ q) -> EstimateQ q) -> Sigma
forallEstimate i p =
     limit (\n ->
       let test_interval u v = Approximation (Interval u v) (let w = midpoint u v in Interval w w)
           p2 x = limit $ \k -> approximate (p x) (k+1)
           sweep [] = limit $ \n -> Approximation True True
           sweep ((Interval a b):xs) = sand (forallEstimate (ClosedInterval(a,b)) p2) (sweep xs)
       in case approximate (estimate p i) n of
            Approximation (x:xs) _ -> Approximation False False
            Approximation [] xs -> if n <= 0 then Approximation False True
                                   else approximate (sweep xs) (n-1)
     )

instance Ord q => Lattice [Interval q] where
  sand _ [] = []
  sand [] _ = []
  sand ((Interval a1 b1):c1) ((Interval a2 b2):c2)
      | a1 > a2 = sand ((Interval a2 b2):c2) ((Interval a1 b1):c1)
      | a2 <= b1 = (Interval a2 b1):(sand c1 ((Interval b1 b2):c2))
      | otherwise = sand c1 ((Interval a2 b2):c2)
  sor x [] = x
  sor [] x = x
  sor ((Interval a1 b1):c1) ((Interval a2 b2):c2)
      | a1 > a2 = sor ((Interval a2 b2):c2) ((Interval a1 b1):c1)
      | a2 <= b1 = sor ((Interval a1 b2):c2) c1
      | otherwise = (Interval a1 b1):(sor ((Interval a2 b2):c2) c1)

instance Lattice Estimate where
  sand = lift2 (\n -> sor)
  sor = lift2 (\n -> sand) --and,or are inverted because we're working on closed intervals


instance DyadicField q => LinearOrder (Forward (RealNumQ q,RealNumQ q)) (EstimateQ q) where
   less a b =   
     limit (\n ->
      let sgn q = compare q zero
          valueapp = fst (primal b - primal a) 
          derivativeapp = snd (tangent b - tangent a)
          Interval lf uf = Data.Reals.Staged.lower $ approximate valueapp n
          Interval ld ud = Data.Reals.Staged.lower $ approximate derivativeapp n
          divU = appDiv (precUp n)
          divD = appDiv (precDown n)          
          upr = case (sgn lf, sgn ld, sgn ud) of
                      (LT, GT, _)  -> [Interval (lf `divU` ld) posInf] -- 0.6 < x
                      (LT, EQ, _)  -> [Interval negInf posInf]  -- 0.5 < x^2
                      (LT, _,  LT) -> [Interval negInf (lf `divD` ud)] -- x < 0.4
                      (LT, _,  _)  -> [Interval negInf posInf] -- (x*x) < -0.5, (x-0.5)^2 < -0.5 
                      (_,  GT, _)  -> [Interval (lf `divU` ud) posInf] -- 0.5 < x
                      (_,  EQ, _)  -> [Interval (lf `divU` ud) posInf] -- 0 < x*x 
                      (_,  _,  LT) -> [Interval negInf (lf `divD` ld)] -- x < 0.5
                      (_,  _,  EQ) -> [Interval negInf (lf `divD` ld)] -- (x*x) < 0.5
                      (_,  _,  _)  -> sor [Interval negInf (lf `divD` ld)] [Interval (lf `divU` ud) posInf]  -- (x-0.5)^2- < 0, (x-0.5)^2 < 0.5-}
          lwr = case (sgn uf, sgn ld, sgn ud) of
                      (GT, GT, _)  -> [Interval (uf `divU` ld) posInf]
                      (GT, EQ, _)  -> []
                      (GT, _,  LT) -> [Interval negInf (uf `divD` ud)] --missing test
                      (GT, _,  EQ) -> []
                      (GT, _,  _)  -> []
                      (_,  GT, _)  -> [Interval (uf `divU` ud) posInf]
                      (_,  EQ, _)  -> [Interval (uf `divU` ud) posInf]
                      (_,  _,  LT) -> [Interval negInf (uf `divD` ld)]
                      (_,  _,  EQ) -> [Interval negInf posInf]
                      (_,  _,  _)  -> [Interval  (uf `divU` ud) (uf `divD` ld)]

        in Approximation lwr upr
     )
     
{- | Estimate may be done using the derivative of the function. --}
estimate :: DyadicField q => (Forward (RealNumQ q,RealNumQ q) -> EstimateQ q) -> ClosedInterval q -> EstimateQ q
estimate f (ClosedInterval (x,y)) = 
    limit (\n ->
      let xm = midpoint x y
          xmint = Interval xm xm
          xmi = embed xmint
          i = (limit $ \n -> (Approximation (Interval x y) (Interval y x)))
          xsub = appSub (precUp n) xmint
          xand (Interval a b) = Interval (max x a) (min y b)
          flt = filter (\(Interval a b) -> a <= b)
          Approximation ls us = approximate (apply f (xmi,i)) n
      in Approximation (flt $ fmap (xand.xsub) ls) (flt $ fmap (xand.xsub) us)
    )
    
    
