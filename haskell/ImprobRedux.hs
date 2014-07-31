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
conditionalexpectation event pmf = expectation (conditionalpmf event pmf)
conditionalexpectations event = lift2 (conditionalexpectation event)
mapexpectations :: ([a] -> a) -> ([[a]] -> [[a]]) -> [[a]] -> [a]
mapexpectations f exps rvars = map f $ exps rvars
lowerprevisions :: Ord a => ([[a]] -> [[a]]) -> [[a]] -> [a]
lowerprevisions = mapexpectations minimum
upperprevisions :: Ord a => ([[a]] -> [[a]]) -> [[a]] -> [a]
upperprevisions = mapexpectations maximum
hurwicz :: (Num a, Ord a) => a -> [a] -> a
hurwicz opt xs = opt * (maximum xs) + (1 - opt) * (minimum xs)
hurwiczprevisions opt = mapexpectations (hurwicz opt)
isgammamaxisomething :: (Num a, Ord a) => a -> ([[a]] -> [a]) -> [[a]] -> [Bool]
isgammamaxisomething tol f rvars = map ismax xs
  where xs = f rvars
        ismax x = x >= (maximum xs) - tol
