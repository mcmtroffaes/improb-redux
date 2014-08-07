module ImprobRedux (
  expectation,
  conditionalpmf,
  conditionalexpectation,
  lowerprevision,
  upperprevision,
  hurwiczprevision,
  isgammamaxisomething,
  rbayesdominates,
  intervaldominates,
  --ismaximal,
  ) where

expectation :: Num a => [a] -> [a] -> a
expectation pmf rvar = sum $ zipWith (*) pmf rvar

conditionalpmf :: Fractional a => [Bool] -> [a] -> [a]
conditionalpmf event pmf = map (/ norm) restrictedpmf
  where restrictedpmf = map fst $ filter snd $ zip pmf event
        norm = sum restrictedpmf

conditionalexpectation :: Fractional a => [Bool] -> [a] -> [a] -> a
conditionalexpectation event pmf = expectation (conditionalpmf event pmf)

mapexpectations :: Num a => ([a] -> b) -> [[a]] -> [a] -> b
mapexpectations f pmfs rvar = f $ map (\pmf -> expectation pmf rvar) pmfs

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

rbayesdominates :: (Num a, Ord a) => a -> [a] -> [a] -> Bool
rbayesdominates tol xs ys = all cmp $ zip xs ys
  where cmp (x, y) = x > y + tol

intervaldominates :: (Num a, Ord a) => a -> [a] -> [a] -> Bool
intervaldominates tol xs ys = minimum xs > maximum ys + tol

{-
ismaximal :: (a -> a -> Bool) -> (b -> [a]) -> b -> [Bool]
ismaximal dominates exps rvars = map (not . isdominated) table
  where table = exps rvars
        isdominated x = any (`dominates` x) table
-}
