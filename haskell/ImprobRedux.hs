module ImprobRedux (
  expectation,
  expectations,
  conditionalpmf,
  conditionalexpectation,
  conditionalexpectations,
  lowerprevisions,
  upperprevisions,
  hurwiczprevisions,
  isgammamaxisomething,
  rbayesdominates,
  intervaldominates,
  ismaximal,
  ) where

lift2 :: (a -> b -> c) -> [a] -> [b] -> [[c]]
lift2 f xs ys = map (\y -> map (\x -> f x y) xs) ys

expectation :: Num a => [a] -> [a] -> a
expectation pmf rvar = sum $ zipWith (*) pmf rvar

expectations :: Num a => [[a]] -> [[a]] -> [[a]]
expectations = lift2 expectation

conditionalpmf :: Fractional a => [Bool] -> [a] -> [a]
conditionalpmf event pmf = map (/ norm) restrictedpmf
  where restrictedpmf = map fst $ filter snd $ zip pmf event
        norm = sum restrictedpmf

conditionalexpectation :: Fractional a => [Bool] -> [a] -> [a] -> a
conditionalexpectation event pmf = expectation (conditionalpmf event pmf)

conditionalexpectations :: Fractional a => [Bool] -> [[a]] -> [[a]] -> [[a]]
conditionalexpectations event = lift2 (conditionalexpectation event)

mapexpectations :: (a -> b) -> (c -> [a]) -> c -> [b]
mapexpectations f exps rvars = map f $ exps rvars

lowerprevisions :: Ord a => (b -> [[a]]) -> b -> [a]
lowerprevisions = mapexpectations minimum

upperprevisions :: Ord a => (b -> [[a]]) -> b -> [a]
upperprevisions = mapexpectations maximum

hurwicz :: (Num a, Ord a) => a -> [a] -> a
hurwicz opt xs = opt * (maximum xs) + (1 - opt) * (minimum xs)

hurwiczprevisions :: (Num a, Ord a) => a -> ([[a]] -> [[a]]) -> [[a]] -> [a]
hurwiczprevisions opt = mapexpectations (hurwicz opt)

isgammamaxisomething :: (Num a, Ord a) => a -> (b -> [a]) -> b -> [Bool]
isgammamaxisomething tol f rvars = map ismax xs
  where xs = f rvars
        ismax x = x >= (maximum xs) - tol

rbayesdominates :: (Num a, Ord a) => a -> [a] -> [a] -> Bool
rbayesdominates tol xs ys = all cmp $ zip xs ys
  where cmp (x, y) = x > y + tol

intervaldominates :: (Num a, Ord a) => a -> [a] -> [a] -> Bool
intervaldominates tol xs ys = minimum xs > maximum ys + tol

ismaximal :: (a -> a -> Bool) -> (b -> [a]) -> b -> [Bool]
ismaximal dominates exps rvars = map (not . isdominated) table
  where table = exps rvars
        isdominated x = any (`dominates` x) table
