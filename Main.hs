import Util
import Chain
import Matrix
import SimplicialComplex
--import Data.List
--import Control.Parallel

main = do
  putStrLn "before:\n[[1,2,3],[4,2,0],[5,7,11]]"
  let after = findAndImprovePivot (Matrix [[1,2,3],[4,2,0],[5,7,11]] 0)
  putStrLn " "
  putStrLn "after: "
  putStrLn $ show after