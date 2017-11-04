module HomCalc where

import Util
import Chain
import Matrix
import SimplicialComplex
import Data.List

main = do
  putStrLn "before:\n[[1,0,3,22,-4],[2,0,0,-18,9],[3,0,15,27,54],[4,0,-16,3,1],[5,0,8,1,9]]"
  let after = findAndImprovePivot (Matrix [[1,0,3,22,4],[2,0,0,18,9],[3,0,15,27,54],[4,0,16,3,1],[5,0,8,1,9]] 0)
  putStr "after: "
  putStrLn $ show after