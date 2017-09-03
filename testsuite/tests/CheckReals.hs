
import Test.HUnit

import Debug.Trace
import Data.Ratio
import Data.Approximate.Floating.MPFR
import Data.Approximate.Interval
import Data.Approximate.ApproximateField
import Data.Reals.Reals
import Data.Reals.Space
import Data.Reals.Staged
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

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

i = ClosedInterval (appFromInteger 0 :: Rounded, appFromInteger 1)

prop_pos1 x = xr `inside` (Data.Reals.Staged.upper e) ==> (f x :: Bool)
  where 
      types = x :: (Ratio Integer)
      xr = appFromRational (precUp 52) x :: Rounded
      e = approximate (estimate f i :: Estimate) 52
      f x = (x * (1 - x)) `less` (0.4)

prop_pos2 x = xr `inside` (Data.Reals.Staged.upper e) ==> (f x :: Bool)
  where 
      types = x :: (Ratio Integer)
      xr = appFromRational (precUp 52) x :: Rounded
      e = approximate (estimate f i :: Estimate) 52
      f x = x `less` (0.5)

prop_pos3 x = xr `inside` (Data.Reals.Staged.upper e) ==> (f x :: Bool)
  where 
      types = x :: (Ratio Integer)
      xr = appFromRational (precUp 52) x :: Rounded
      e = approximate (estimate f i :: Estimate) 52
      f x = (-x) `less` (-0.5)
     
--tests :: Test
tests = hunit ++ qcheck
    where
       hunit = hUnitTestToTests $ TestList [TestLabel "test1" test_1, TestLabel "test2" test_2]
       qcheck = [testProperty "pos1" prop_pos1, testProperty "pos2" prop_pos2, testProperty "pos3" prop_pos3]

--main :: IO Counts
--main = do runTestTT tests
main = defaultMain tests

