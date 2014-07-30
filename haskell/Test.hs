module Test where

import Data.Ratio
import ImprobRedux
import Test.HUnit

test1 = TestCase $ do
  assertEqual
    "conditional expectation"
    [[1, 0], [70 % 75, 5 % 75], [40 % 75, 35 % 75], [4 % 5, 1 % 5], [1, 0]]
    (exps rvars)
  assertEqual
    "lower prevision"
    [-1 % 3]
    (lowerprevisions exps [[2, -3]])
  assertEqual
    "upper prevision"
    [2]
    (upperprevisions exps [[2, -3]])
  where
    pmfs = [
      [ 3 % 10,   7 % 10,   0],
      [25 % 100, 70 % 100,  5 % 100],
      [25 % 100, 40 % 100, 35 % 100],
      [ 5 % 10,   4 % 10,   1 % 10],
      [ 5 % 10,   5 % 10,   0]]
    event = [False, True, True]
    rvars = [[1, 0], [0, 1]]
    exps = conditionalexpectations event pmfs

main = do
  runTestTT $ TestList [test1]
  putStrLn $ show $ expectations [[0.1, 0.9], [0.5, 0.5]] [[2, 3], [4, 1]]
  putStrLn $ show $ conditionalexpectations
    [True, False, True] [[0.1, 0.7, 0.2], [0.2, 0.5, 0.3]] [[3, 4], [2, 0]]
  putStrLn $ show $ lowerprevisions exps [[2, 3], [4, 1]]
  putStrLn $ show $ upperprevisions exps [[2, 3], [4, 1]]
  putStrLn $ show $ hurwiczprevisions 0.5 exps [[2, 3], [4, 1]]
  putStrLn $ show $ isgammamaximax [[2, 3], [4, 1]]
  putStrLn $ show $ isgammamaximin [[2, 3], [4, 1]]
  where exps = expectations [[0.1, 0.9], [0.5, 0.5]]
        lpr = lowerprevisions exps
        upr = upperprevisions exps
        isgammamaximax = isgammamaxisomething 1e-10 lpr
        isgammamaximin = isgammamaxisomething 1e-10 upr
