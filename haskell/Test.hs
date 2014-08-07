module Test where

import Data.Ratio
import ImprobRedux
import Test.HUnit

test1 = TestCase $ do
  assertEqual
    "conditional pmfs for [False, True, True]"
    [[1, 0],
     [70 % 75, 5 % 75],
     [40 % 75, 35 % 75],
     [4 % 5, 1 % 5]]
    cpmfs
  assertEqual
    "lower prevision"
    (-1 % 3)
    (lowerPrevision cpmfs [2, -3])
  assertEqual
    "upper prevision"
    (2 % 1)
    (upperPrevision cpmfs [2, -3])
  where
    event = [False, True, True]
    pmfs = [
      [ 3 % 10,   7 % 10,   0],
      [25 % 100, 70 % 100,  5 % 100],
      [25 % 100, 40 % 100, 35 % 100],
      [ 5 % 10,   4 % 10,   1 % 10],
      [ 5 % 10,   5 % 10,   0]]
    cpmfs = conditionalPmfs event pmfs

test2 =  TestCase $ do
  assertEqual
    "conditional pmfs for [True, False, False]"
    [[1 % 1]]
    (conditionalPmfs event pmfs)
  where
    event = [True, False, False]
    pmfs = [
      [ 3 % 10,   7 % 10,   0],
      [25 % 100, 70 % 100,  5 % 100],
      [25 % 100, 40 % 100, 35 % 100],
      [ 5 % 10,   4 % 10,   1 % 10],
      [ 5 % 10,   5 % 10,   0]]

test3 = TestCase $ do
  assertEqual
    "expectation for pmf = [0.4, 0.6] and rvar = [3, 9]"
    (66 % 10)
    (expectation [4 % 10, 6 % 10] [3, 9])

test4 = TestCase $ do
  assertEqual
    "expectation for pmf = [0.4, 0.6] and multiple rvars"
    [66 % 10, 4, 18 % 10]
    (map (expectation [4 % 10, 6 % 10]) [[3, 9], [4, 4], [0, 3]])

test6 = TestCase $ do
  assertEqual "lower previsions"
    [4, 4, 18 % 10, 23 % 10]
    (map (lowerPrevision pmfs) rvars)
  assertEqual "upper previsions"
    [77 % 10, 4, 3, 42 % 10]
    (map (upperPrevision pmfs) rvars)
  assertEqual "hurwicz previsions"
    [585 % 100, 4, 24 % 10, 325 % 100]
    (map (hurwiczPrevision (1 % 2) pmfs) rvars)
  assertEqual "Gamma-maximin"
    [True, True, False, False] (isGammaMaxiMin 0 pmfs rvars)
  assertEqual "Gamma-maximax"
    [True, False, False, False] (isGammaMaxiMax 0 pmfs rvars)
  assertEqual "hurwicz"
    [True, False, False, False] (isHurwicz 0 (1 % 2) pmfs rvars)
  assertEqual "rbayes maximal"
    [True, True, False, True] (isRBayesMaximal 0 pmfs rvars)
  assertEqual "interval maximal"
    [True, True, False, True] (isIntervalMaximal 0 pmfs rvars)
  where
    rvars = [[3, 9, 2], [4, 4, 4], [0, 3, 6], [6, 2, 1]]
    pmfs =
      [[4 % 10, 5 % 10, 1 % 10],
       [1 % 10, 8 % 10, 1 % 10],
       [6 % 10, 2 % 10, 2 % 10]]

main = do
  runTestTT $ TestList [test1, test2, test3, test4, test6]
