module ImprobRedux (
  conditionalpmf,
  conditionalpmfs,
  expectation,
  lowerprevision,
  upperprevision,
  hurwiczprevision,
  isgammamaximin,
  isgammamaximax,
  ishurwicz,
  isrbayesmaximal,
  isintervalmaximal,
  ) where

import Data.List (nub)

conditionalpmf :: Fractional a => [Bool] -> [a] -> [a]
conditionalpmf event pmf = map (/ norm) restrictedpmf
  where restrictedpmf = map fst $ filter snd $ zip pmf event
        norm = sum restrictedpmf

conditionalpmfs :: (Eq a, Fractional a) => [Bool] -> [[a]] -> [[a]]
conditionalpmfs event pmfs = nub $ map (conditionalpmf event) pmfs

expectation :: Num a => [a] -> [a] -> a
expectation pmf rvar = sum $ zipWith (*) pmf rvar

mapexpectations :: Num a => ([a] -> b) -> [[a]] -> [a] -> b
mapexpectations f pmfs rvar = f $ map (flip expectation rvar) pmfs

lowerprevision :: (Num a, Ord a) => [[a]] -> [a] -> a
lowerprevision = mapexpectations minimum

upperprevision :: (Num a, Ord a) => [[a]] -> [a] -> a
upperprevision = mapexpectations maximum

hurwicz :: (Num a, Ord a) => a -> [a] -> a
hurwicz opt xs = opt * (maximum xs) + (1 - opt) * (minimum xs)

hurwiczprevision :: (Num a, Ord a) => a -> [[a]] -> [a] -> a
hurwiczprevision opt = mapexpectations (hurwicz opt)

isgammamaxisomething :: (Num a, Ord a) => a -> ([a] -> a) -> [[a]] -> [Bool]
isgammamaxisomething tol f rvars = map ismax xs
  where xs = map f rvars
        ismax x = x >= (maximum xs) - tol

isgammamaximin :: (Num a, Ord a) => a -> [[a]] -> [[a]] -> [Bool]
isgammamaximin tol pmfs = isgammamaxisomething tol (lowerprevision pmfs)

isgammamaximax :: (Num a, Ord a) => a -> [[a]] -> [[a]] -> [Bool]
isgammamaximax tol pmfs = isgammamaxisomething tol (upperprevision pmfs)
ishurwicz tol opt pmfs = isgammamaxisomething tol (hurwiczprevision opt pmfs)

pointwisedominates :: (Num a, Ord a) => a -> [a] -> [a] -> Bool
pointwisedominates tol xs ys = all cmp $ zip xs ys
  where cmp (x, y) = x > y + tol

intervaldominates :: (Num a, Ord a) => a -> [a] -> [a] -> Bool
intervaldominates tol xs ys = minimum xs > maximum ys + tol

ismaximal :: Num a => ([a] -> [a] -> Bool) -> [[a]] -> [[a]] -> [Bool]
ismaximal dominates pmfs rvars = map (not . isdominated) table
  where table = map (mapexpectations id pmfs) rvars
        isdominated x = any (`dominates` x) table

isrbayesmaximal :: (Num a, Ord a) => a -> [[a]] -> [[a]] -> [Bool]
isrbayesmaximal tol = ismaximal (pointwisedominates tol)

isintervalmaximal :: (Num a, Ord a) => a -> [[a]] -> [[a]] -> [Bool]
isintervalmaximal tol = ismaximal (intervaldominates tol)
