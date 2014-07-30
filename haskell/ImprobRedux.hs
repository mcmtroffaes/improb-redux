lift2 f xs ys = map (\y -> map (\x -> f x y) xs) ys
expectation pmf rvar = sum $ zipWith (*) pmf rvar
expectations = lift2 expectation
conditionalpmf event pmf = map (/ norm) restrictedpmf
  where restrictedpmf = map fst $ filter snd $ zip pmf event
        norm = sum restrictedpmf
conditionalexpectation event pmf = expectation (conditionalpmf event pmf)
conditionalexpectations event = lift2 (conditionalexpectation event)
mapexpectations f exp rvars = map f $ exp rvars
lowerprevisions = mapexpectations minimum
upperprevisions = mapexpectations maximum
hurwicz opt xs = opt * (maximum xs) + (1 - opt) * (minimum xs)
hurwiczprevisions opt = mapexpectations (hurwicz opt)
isgammamaxisomething tol f rvars = map (\x -> (x >= mx - tol)) xs
  where xs = f rvars
        mx = maximum xs

main = do
  putStrLn $ show $ expectation [0.1, 0.9] [2, 3]
  putStrLn $ show $ expectations [[0.1, 0.9], [0.5, 0.5]] [[2, 3], [4, 1]]
  putStrLn $ show $ conditionalpmf [True, False, True] [0.1, 0.7, 0.2]
  putStrLn $ show $ conditionalpmf [True, False, True] [0.2, 0.5, 0.3]
  putStrLn $ show $ conditionalexpectation
    [True, False, True] [0.1, 0.7, 0.2] [3, 4]
  putStrLn $ show $ conditionalexpectations
    [True, False, True] [[0.1, 0.7, 0.2], [0.2, 0.5, 0.3]] [[3, 4], [2, 0]]
  putStrLn $ show $ lowerprevisions exp [[2, 3], [4, 1]]
  putStrLn $ show $ upperprevisions exp [[2, 3], [4, 1]]
  putStrLn $ show $ hurwiczprevisions 0.5 exp [[2, 3], [4, 1]]
  putStrLn $ show $ isgammamaximax [[2, 3], [4, 1]]
  putStrLn $ show $ isgammamaximin [[2, 3], [4, 1]]
  where exp = expectations [[0.1, 0.9], [0.5, 0.5]]
        lpr = lowerprevisions exp
        upr = upperprevisions exp
        isgammamaximax = isgammamaxisomething 1e-10 lpr
        isgammamaximin = isgammamaxisomething 1e-10 upr