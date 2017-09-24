{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Test.HUnit

import Debug.Trace
import Data.Ratio
import Data.Approximate.Floating.MPFR
import Data.Approximate.Interval
import Data.Approximate.ApproximateField
import Data.Reals.Reals
import Data.Reals.Space
import Data.Reals.Staged
import Data.Reals.Estimates
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

instance Ord a => LinearOrder a Bool where
   less a b = a < b

assertIn a b x = assertBool 
      ("Result " ++ show x ++ " in interval [" ++ show a ++ "," ++ show b ++ "]")
      (x > a && x < b)

--test_1 :: Test
test_1 = 
  TestCase (do
   --    putStrLn $ "a = " ++ show a
   --    putStrLn $ "a * (1-a) = " ++ show (a * (1-a))
       assertIn (-0.4) (-0.38) $ a * (1-a)

       --putStrLn $ "a^200 = " ++ show (a ^ 200)
       assertIn 6.144e22 6.149e22 (a ^ 200)
       )
     where
       prod = a * (1-a)
       a = 1.3 :: RealNum

--test_2 :: Test
test_2 = TestCase (do
 --     putStrLn $ "a = " ++ show a
 --     putStrLn $ "b = " ++ show b
      assertBool "forall x:[0,1].x*(1-x)<0.26" (force a) 
      assertBool "forall x:[0,1].x*(1-x)<0.24" (not $ force b)
      )
    where
      i = ClosedInterval (appFromInteger 0 :: Rounded, appFromInteger 1)
      a = forall i $ \x -> (x * (1 - x)) `less` (0.26) :: Estimate
      b = forall i $ \x -> (x * (1 - x)) `less` (0.24) :: Estimate
      
inside x [] = False      
inside x (l:ls) = (x >= (Data.Approximate.Interval.lower l) && x <= (Data.Approximate.Interval.upper l)) || inside x ls
outside x s = not $ inside x s

i = Interval (appFromInteger 0 :: Rounded) (appFromInteger 1)
i2 = Interval{Data.Approximate.Interval.lower=appFromInteger 0 :: Rounded, Data.Approximate.Interval.upper=appFromInteger 1}

prop_upper :: (forall t s.(Fractional t, LinearOrder t s) => (t -> s)) -> Rational -> Property
prop_upper f x = xr `inside` [i2] ==> (xr `outside` (Data.Reals.Staged.upper e) <= (f x :: Bool))
  where 
      types = x :: (Ratio Integer)
      xr = appFromRational (precUp 52) x :: Rounded
      e = approximate (estimate f i :: Estimate) 52

prop_lower :: (forall t s.(Fractional t, LinearOrder t s) => (t -> s)) -> Rational -> Property
prop_lower f x = xr `inside` [i2] ==> (xr `inside` (Data.Reals.Staged.lower e) <= not (f x :: Bool))
  where 
      types = x :: (Ratio Integer)
      xr = appFromRational (precUp 52) x :: Rounded
      e = approximate (estimate f i :: Estimate) 52

test_upper :: String -> (forall t s.(Fractional t, LinearOrder t s) => (t -> s)) -> Test.Framework.Test
test_upper name f = testProperty name (prop_upper f)

qcheck = [
   test_upper "upper GGGG" (\x -> 0 `less` x),
   test_upper "upper GGLG" (\x -> ((x - 0.5)*(x - 0.5)) `less` (0.5)),
   test_upper "upper GGLG" (\x -> (x * (1 - x)) `less` (0.4)),
   test_upper "upper EELL" (\x -> x `less` (0.5)),
   test_upper "upper LLGG" (\x -> 0.6 `less` x),
   test_upper "upper LLEG" (\x -> 0.5 `less` (x*x)),
   test_upper "upper LLLL" (\x -> x `less` 0.4),
   test_upper "upper LLLE" (\x -> (x*x) `less` (-0.5)),
   test_upper "upper EEGG" (\x -> 0.5 `less` x),
   test_upper "upper GGEG" (\x -> 0 `less` (x*x)),
   test_upper "upper GGLE" (\x -> (x*x) `less` 0.5),
   test_upper "upper EELL" (\x -> x `less` 0.5),
   test_upper "upper LLLG" (\x -> ((x - 0.5)*(x - 0.5)) `less` (-0.5)),
   test_upper "upper EELG" (\x -> ((x - 0.5)*(x - 0.5)) `less` 0)
  ]

--tests :: Test
tests = hunit ++ qcheck
    where
       hunit = hUnitTestToTests $ TestList [TestLabel "test1" test_1, TestLabel "test2" test_2]


--main :: IO Counts
--main = do runTestTT tests
main = defaultMain tests

