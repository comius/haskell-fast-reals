
import Test.HUnit

import Data.Approximate.Floating.MPFR
import Data.Approximate.ApproximateField
import Data.Reals.Reals
import Data.Reals.Space
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck
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
      
prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Int]
     
test_4 = TestCase (do
                quickCheck prop_RevRev          
        )
      
{-test_3 = TestCase (do
      assertBool "forall x:[0,1].x*(1-x)<0.26" (force a) 
      assertBool "forall x:[0,1].x*(1-x)<0.24" (not $ force b)
      )
-}

--tests :: Test
tests = hunit ++ qcheck
    where
       hunit = hUnitTestToTests $ TestList [TestLabel "test1" test_1, TestLabel "test2" test_2]--, TestLabel "qtest" test_4]
       qcheck = [testProperty "revrev" prop_RevRev]

--main :: IO Counts
--main = do runTestTT tests
main = defaultMain tests

