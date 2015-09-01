-- |

module TestReals where

import Control.Exception
import Distribution.TestSuite
import DyadicRounded
import Reals
import Space

assertIn a b x = if x > a && x < b
  then return ()
   else throwIO $ ErrorCall $
     "Result " ++ (show x) ++ " not in interval [" ++  (show a) ++ "," ++ (show b) ++ "]";

assertBool msg b = if b
                      then return ()
                      else throwIO $ ErrorCall $ "Assertion failed " ++ msg

mainCatch t
  = catch t ( \e -> return $ Finished $ Fail (show (e::SomeException)) )

test1 = do
       putStrLn $ "a = " ++ (show a)
       putStrLn $ "a * (1-a) = " ++ show (a * (1-a))
       assertIn (-0.4) (-0.38) $ a * (1-a)

       putStrLn $ "a^200 = " ++ show (a ^ 200)
       assertIn 6.144e22 6.149e22 (a ^ 200)

       return $ Finished Pass
  where
       prod = a * (1-a)
       a = exact 1.3

test2 = do
      putStrLn $ "a = " ++ (show a)
      putStrLn $ "b = " ++ (show b)
      assertBool "forall x:[0,1].x*(1-x)<0.26" (force a)
      assertBool "forall x:[0,1].x*(1-x)<0.24" (not $ force b)

      return $ Finished Pass
    where
      a = forall (ClosedInterval (0,1)) $ \x -> (x * (1 - x)) `less` exact 0.26
      b = forall (ClosedInterval (0,1)) $ \x -> (x * (1 - x)) `less` exact 0.24



test f name = Test testInstance
   where
    testInstance = TestInstance
     { run = mainCatch f
     , name = name
     , tags = []
     , options = []
     , setOption = \_ _ -> Right testInstance
     }
tests :: IO [Test]
tests = return [ test test1 "test1", test test2 "test2"  ]
