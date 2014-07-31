module Test where

import Data.Ratio
import ImprobRedux
import Test.HUnit

test1 = TestCase $ do
  assertEqual
    "conditional expectation for [False, True, True]"
    [[1, 70 % 75, 40 % 75, 4 % 5, 1],
     [0,  5 % 75, 35 % 75, 1 % 5, 0]]
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
    event = [False, True, True]
    rvars = [[1, 0], [0, 1]]
    pmfs = [
      [ 3 % 10,   7 % 10,   0],
      [25 % 100, 70 % 100,  5 % 100],
      [25 % 100, 40 % 100, 35 % 100],
      [ 5 % 10,   4 % 10,   1 % 10],
      [ 5 % 10,   5 % 10,   0]]
    exps = conditionalexpectations event pmfs

test2 =  TestCase $ do
  assertEqual
    "conditional expectation for [True, False, False]"
    [[1,1,1,1,1], [2,2,2,2,2], [3,3,3,3,3]]
    (exps rvars)
  where
    event = [True, False, False]
    rvars = [[1], [2], [3]]
    pmfs = [
      [ 3 % 10,   7 % 10,   0],
      [25 % 100, 70 % 100,  5 % 100],
      [25 % 100, 40 % 100, 35 % 100],
      [ 5 % 10,   4 % 10,   1 % 10],
      [ 5 % 10,   5 % 10,   0]]
    exps = conditionalexpectations event pmfs

test3 = TestCase $ do
  assertEqual
    "expectation for pmf = [0.4, 0.6] and rvar = [3, 9]"
    (66 % 10)
    (expectation [4 % 10, 6 % 10] [3, 9])

test4 = TestCase $ do
  assertEqual
    "expectation for pmf = [0.4, 0.6] and multiple rvars"
    [[66 % 10], [4], [18 % 10]]
    (expectations [[4 % 10, 6 % 10]] [[3, 9], [4, 4], [0, 3]])

test5 = TestCase $ do
  assertEqual
    "expectation of multiple random variables with multiple pmfs"
    [[59 % 10, 77 % 10], [4, 4], [21 % 10, 3], [35 % 10, 23 % 10]]
    (expectations
      [[4 % 10, 5 % 10, 1 % 10], [1 % 10, 8 % 10, 1 % 10]]
      [[3, 9, 2], [4, 4, 4], [0, 3, 6], [6, 2, 1]])

test6 = TestCase $ do
  assertEqual
    "expectation table"
    [[59 % 10, 77 % 10, 4],
     [4, 4, 4],
     [21 % 10, 3, 18 % 10],
     [35 % 10, 23 % 10, 42 % 10]]
    (exps rvars)
  assertEqual "lower previsions" [4, 4, 18 % 10, 23 % 10] (lprs rvars)
  assertEqual "upper previsions" [77 % 10, 4, 3, 42 % 10] (uprs rvars)
  assertEqual "hurwicz previsions" [585 % 100, 4, 24 % 10, 325 % 100] (hprs rvars)
  assertEqual "Gamma-maximin" [True, True, False, False] (isgammamaximin rvars)
  assertEqual "Gamma-maximax" [True, False, False, False] (isgammamaximax rvars)
  assertEqual "hurwicz" [True, False, False, False] (ishurwicz rvars)
  assertEqual "rbayes maximal" [True, True, False, True] (isrbayesmaximal rvars)
  assertEqual "interval maximal" [True, True, False, True] (isintervalmaximal rvars)
  where
    rvars = [[3, 9, 2], [4, 4, 4], [0, 3, 6], [6, 2, 1]]
    exps = expectations
      [[4 % 10, 5 % 10, 1 % 10],
       [1 % 10, 8 % 10, 1 % 10],
       [6 % 10, 2 % 10, 2 % 10]]
    lprs = lowerprevisions exps
    uprs = upperprevisions exps
    hprs = hurwiczprevisions 0.5 exps
    isgammamaximin = isgammamaxisomething 0 lprs
    isgammamaximax = isgammamaxisomething 0 uprs
    ishurwicz = isgammamaxisomething 0 hprs
    isrbayesmaximal = ismaximal (rbayesdominates 0) exps
    isintervalmaximal = ismaximal (intervaldominates 0) exps

main = do
  runTestTT $ TestList [test1, test2, test3, test4, test5, test6]
