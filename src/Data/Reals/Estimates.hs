{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

module Data.Reals.Estimates (
             forall, estimate, Estimate, forallEstimate, existsEstimate, complement, cutEstimate
) where

import Data.Approximate.Floating.MPFR 
import Data.Reals.Staged
import Data.Reals.Reals
import Data.Approximate.ApproximateField
import Data.Approximate.Interval
import Data.Reals.Space
import Data.NumInstances
import Debug.Trace

type EstimateQ q = StagedWithFun ([Interval q])
type Estimate = EstimateQ Rounded

instance DyadicField q => Compact (ClosedInterval q) (Forward (RealNumQ q,RealNumQ q)) (EstimateQ q) where
   forall (ClosedInterval (a,b)) p = forallEstimate p (Interval a b)


data Forward t = Forward { primal :: t, tangent :: t}


instance Num a => Num (Forward a) where
   negate x   = Forward {primal = negate (primal x), tangent = negate (tangent x)}
   x + y = Forward {primal = (primal x)+(primal y), tangent = (tangent x) + tangent y}
   x * y = Forward {primal = (primal x)*(primal y), tangent = (primal x)*(tangent y)+(tangent x)*(primal y)}
   fromInteger i   = Forward {primal = fromInteger i, tangent = fromInteger 0}

apply f x = f (Forward {primal = x, tangent = 1})

forallEstimate :: DyadicField q => (Forward (RealNumQ q, RealNumQ q) -> EstimateQ q) -> Interval q -> Sigma
forallEstimate p i =
     limit (\n ->
       let p2 x = limit $ \k -> approximate (p x) (k+5) -- TODO optimize this spot
           sweep lst = foldr sand (embed True) (map (forallEstimate p2) lst)
           a = approximate (estimate p i) n
       in case a of 
            Approximation (x:xs) _ -> traceShow("nw", x:xs) $ Approximation False False -- negative example
            Approximation [] [] -> Approximation True True  -- not unsure
            Approximation [] xs -> if n <= 0 then Approximation False True -- need to recurse, possibly bisect
                                   else approximate (sweep xs) (n-1)
     )


existsEstimate :: DyadicField q => (Forward (RealNumQ q, RealNumQ q) -> EstimateQ q) -> Interval q -> Sigma
existsEstimate p i =
     limit (\n ->
       let p2 x = limit $ \k -> approximate (p x) (k+5) -- TODO optimize this spot
           sweep lst = foldr sor (embed False) (map (existsEstimate p2) lst)
           a = approximate (estimate p i) n
       in case a of  -- (not true) (don't know)
            Approximation l1 _  | l1 == [i] -> Approximation False False -- there can be no witness
            Approximation l1 l2 |  complement i (sor l1 l2) /= [] -> traceShow ("w",complement i (sor l1 l2)) $ Approximation True True -- the witness
            Approximation l1 l2 -> let rest = sand (complement i l1) l2
                                   in if n <= 0 then Approximation False True 
                                        else approximate (sweep rest) (n-1)
     )


cutEstimate :: DyadicField q => (Forward (RealNumQ q, RealNumQ q) -> EstimateQ q) -> (Forward (RealNumQ q, RealNumQ q) -> EstimateQ q) -> Interval q -> RealNumQ q
cutEstimate l u i =
     let find i n = find2 i n n
         find2 i 0 _ = Approximation i i
         find2 (Interval a b) n m = find2 i2 (n-1) (m+1)
           where Approximation lle ule = approximate (estimate l li) m
                 Approximation lue uue = approximate (estimate u ui) m
                 first [] = Interval b a
                 first (i : xs) = i
                 xm = midpoint a b
                 li = Interval a xm
                 ui = Interval xm b
                 lt = complement li (sor lle ule)
                 ut = complement ui (sor lue uue)
                 Interval _ a2 = first (reverse lt)
                 Interval b2 _ = first ut
                 i2 = Interval a2  b2 -- next interval        
     in limit (find i)


complement (Interval a b) [] | a==b = []
complement (Interval a b) [] = [Interval a b]
complement (Interval a b) ((Interval a1 b1):xs) | a==a1 = complement (Interval b1 b) xs
complement (Interval a b) ((Interval a1 b1):xs) = (Interval a a1):(complement (Interval b1 b) xs)


instance Ord q => Lattice [Interval q] where
  sand _ [] = []
  sand [] _ = []
  sand ((Interval a1 b1):c1) ((Interval a2 b2):c2)
      | a1 > a2 = sand ((Interval a2 b2):c2) ((Interval a1 b1):c1)
      | a2 <= b1 = (Interval a2 (min b1 b2)):(sand c1 ((Interval b1 b2):c2))
      | otherwise = sand c1 ((Interval a2 b2):c2)
  sor x [] = x
  sor [] x = x
  sor ((Interval a1 b1):c1) ((Interval a2 b2):c2)
      | a1 > a2 = sor ((Interval a2 b2):c2) ((Interval a1 b1):c1) -- swap
      | a2 <= b1 = sor ((Interval a1 (max b1 b2)):c2) c1 -- a1 < a2 ... b1 b2
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
estimate :: DyadicField q => (Forward (RealNumQ q,RealNumQ q) -> EstimateQ q) -> Interval q -> EstimateQ q
estimate f (Interval x y) = 
    limit (\n ->
      let xm = midpoint x y
          xmint = Interval xm xm
          xmi = embed xmint
          i = (limit $ \n -> (Approximation (Interval x y) (Interval x y)))
          xsub = appSub (precUp n) xmint
          xsub2 = appSub (precDown n) xmint -- TODO some of the new intervals can touch after this step...
          xand (Interval a b) = Interval (max x a) (min y b)
          flt = filter (\(Interval a b) -> a <= b)
          Approximation ls us = approximate (apply f (xmi,i)) n
      in Approximation (flt $ reverse $ fmap (xand.xsub) ls) (flt $ reverse $ fmap (xand.xsub2) us)
    )
    
    
