module ImprobRedux (
  expectations,
  conditionalexpectations,
  lowerprevisions,
  upperprevisions,
  hurwiczprevisions,
  isgammamaxisomething,
  ) where

lift2 f xs ys = map (\y -> map (\x -> f x y) xs) ys
expectation pmf rvar = sum $ zipWith (*) pmf rvar
expectations = lift2 expectation
conditionalpmf event pmf = map (/ norm) restrictedpmf
  where restrictedpmf = map fst $ filter snd $ zip pmf event
        norm = sum restrictedpmf
conditionalexpectation event pmf = expectation (conditionalpmf event pmf)
conditionalexpectations event = lift2 (conditionalexpectation event)
mapexpectations f exps rvars = map f $ exps rvars
lowerprevisions = mapexpectations minimum
upperprevisions = mapexpectations maximum
hurwicz opt xs = opt * (maximum xs) + (1 - opt) * (minimum xs)
hurwiczprevisions opt = mapexpectations (hurwicz opt)
isgammamaxisomething tol f rvars = map ismax xs
  where xs = f rvars
        ismax x = x >= (maximum xs) - tol
