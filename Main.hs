import Util
--import Chain
import Matrix
import SimplicialComplex
--import Data.List
--import Control.Parallel

main = do
  let matrix :: Matrix Int
      matrix = initializeMatrix False [[2,3,5,7,11],[13,(-17),19,(-23),29],[(-31),37,(-41),43,(-47)],[53,(-59),61,(-67),71],[(-73),79,(-83),89,1]]
  putStrLn "before: "
  putStrLn $ toString matrix
  let after = getSmithNormalForm matrix
  putStrLn " "
  putStrLn "after: "
  putStrLn $ toString $ after