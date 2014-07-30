module Test where

import ImprobRedux

main = do
  putStrLn $ show $ expectations [[0.1, 0.9], [0.5, 0.5]] [[2, 3], [4, 1]]
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
