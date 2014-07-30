-- Lift a function of two arguments to operate on lists. The result is
-- returned as a matrix.
lift2 :: (a -> b -> c) -> [a] -> [b] -> [[c]]
lift2 f xs ys = map (\y -> map (\x -> f x y) xs) ys

expectation :: [Double] -> [Double] -> Double
expectation pmf rvar = sum $ zipWith (*) pmf rvar

expectations :: [[Double]] -> [[Double]] -> [[Double]]
expectations = lift2 expectation

conditionalpmf :: [Double] -> [Bool] -> [Double]
conditionalpmf pmf event = map (/ norm) restrictedpmf
  where restrictedpmf = map fst $ filter snd $ zip pmf event
        norm = sum restrictedpmf

conditionalexpectation :: [Double] -> [Bool] -> [Double] -> Double
conditionalexpectation pmf event rvar =
  expectation (conditionalpmf pmf event) rvar

conditionalexpectations :: [[Double]] -> [Bool] -> [[Double]] -> [[Double]]
conditionalexpectations pmfs event rvars =
  (lift2 (\pmf rvar -> conditionalexpectation pmf event rvar)) pmfs rvars

mapexpectations :: ([Double] -> a)
  -> ([[Double]] -> [[Double]]) -> [[Double]] -> [a]
mapexpectations f exp rvars = map f $ exp rvars

lowerprevisions = mapexpectations minimum
upperprevisions = mapexpectations maximum

hurwicz :: Double -> [Double] -> Double
hurwicz opt xs = opt * (maximum xs) + (1 - opt) * (minimum xs)
hurwiczprevisions opt = mapexpectations (hurwicz opt)

isgammamaxisomething tol f rvars = map (\x -> (x >= mx - tol)) xs
  where xs = f rvars
        mx = maximum xs

main = do
  putStrLn $ show $ expectation [0.1, 0.9] [2, 3]
  putStrLn $ show $ expectations [[0.1, 0.9], [0.5, 0.5]] [[2, 3], [4, 1]]
  putStrLn $ show $ conditionalpmf [0.1, 0.7, 0.2] [True, False, True]
  putStrLn $ show $ conditionalpmf [0.2, 0.5, 0.3] [True, False, True]
  putStrLn $ show $ conditionalexpectation
    [0.1, 0.7, 0.2] [True, False, True] [3, 4]
  putStrLn $ show $ conditionalexpectations
    [[0.1, 0.7, 0.2], [0.2, 0.5, 0.3]] [True, False, True] [[3, 4], [2, 0]]
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
