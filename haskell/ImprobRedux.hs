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

main = do
  putStrLn $ show $ expectation [0.1, 0.9] [2, 3]
  putStrLn $ show $ expectations [[0.1, 0.9], [0.5, 0.5]] [[2, 3], [4, 1]]
  putStrLn $ show $ conditionalpmf [True, False, True] [0.1, 0.7, 0.2]
  putStrLn $ show $ conditionalpmf [True, False, True] [0.2, 0.5, 0.3]
  putStrLn $ show $ conditionalexpectation
    [True, False, True] [0.1, 0.7, 0.2] [3, 4]
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
