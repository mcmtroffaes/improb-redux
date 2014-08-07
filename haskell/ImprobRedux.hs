module ImprobRedux (
  conditionalPmf,
  conditionalPmfs,
  expectation,
  lowerPrevision,
  upperPrevision,
  hurwiczPrevision,
  isGammaMaxiMin,
  isGammaMaxiMax,
  isHurwicz,
  isRBayesMaximal,
  isIntervalMaximal,
  ) where

import Data.List (nub)

conditionalPmf :: Fractional a => [Bool] -> [a] -> [a]
conditionalPmf event pmf = map (/ norm) restrictedpmf
  where restrictedpmf = map fst $ filter snd $ zip pmf event
        norm = sum restrictedpmf

conditionalPmfs :: (Eq a, Fractional a) => [Bool] -> [[a]] -> [[a]]
conditionalPmfs event pmfs = nub $ map (conditionalPmf event) pmfs

expectation :: Num a => [a] -> [a] -> a
expectation pmf rvar = sum $ zipWith (*) pmf rvar

transformExpectations :: Num a => ([a] -> b) -> [[a]] -> [a] -> b
transformExpectations f pmfs rvar = f $ map (flip expectation rvar) pmfs

lowerPrevision :: (Num a, Ord a) => [[a]] -> [a] -> a
lowerPrevision = transformExpectations minimum

upperPrevision :: (Num a, Ord a) => [[a]] -> [a] -> a
upperPrevision = transformExpectations maximum

hurwicz :: (Num a, Ord a) => a -> [a] -> a
hurwicz opt xs = opt * (maximum xs) + (1 - opt) * (minimum xs)

hurwiczPrevision :: (Num a, Ord a) => a -> [[a]] -> [a] -> a
hurwiczPrevision opt = transformExpectations (hurwicz opt)

isGammaMaxiSomething :: (Num a, Ord a) => a -> ([a] -> a) -> [[a]] -> [Bool]
isGammaMaxiSomething tol f rvars = map ismax xs
  where xs = map f rvars
        ismax x = x >= (maximum xs) - tol

isGammaMaxiMin :: (Num a, Ord a) => a -> [[a]] -> [[a]] -> [Bool]
isGammaMaxiMin tol pmfs = isGammaMaxiSomething tol (lowerPrevision pmfs)

isGammaMaxiMax :: (Num a, Ord a) => a -> [[a]] -> [[a]] -> [Bool]
isGammaMaxiMax tol pmfs = isGammaMaxiSomething tol (upperPrevision pmfs)


isHurwicz :: (Num a, Ord a) => a -> a -> [[a]] -> [[a]] -> [Bool]
isHurwicz tol opt pmfs = isGammaMaxiSomething tol (hurwiczPrevision opt pmfs)

pointwiseDominates :: (Num a, Ord a) => a -> [a] -> [a] -> Bool
pointwiseDominates tol xs ys = all cmp $ zip xs ys
  where cmp (x, y) = x > y + tol

intervalDominates :: (Num a, Ord a) => a -> [a] -> [a] -> Bool
intervalDominates tol xs ys = minimum xs > maximum ys + tol

isMaximal :: Num a => ([a] -> [a] -> Bool) -> [[a]] -> [[a]] -> [Bool]
isMaximal dominates pmfs rvars = map (not . isdominated) table
  where table = map (transformExpectations id pmfs) rvars
        isdominated x = any (`dominates` x) table

isRBayesMaximal :: (Num a, Ord a) => a -> [[a]] -> [[a]] -> [Bool]
isRBayesMaximal tol = isMaximal (pointwiseDominates tol)

isIntervalMaximal :: (Num a, Ord a) => a -> [[a]] -> [[a]] -> [Bool]
isIntervalMaximal tol = isMaximal (intervalDominates tol)
