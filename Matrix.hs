--TODO:check out applyColumnOp
module Matrix 
  ( IMatrix
  , BMatrix
  , multiply
  , multiplyPar
  , getSmithNormalFormInt
  , getSmithNormalFormIntPar
  , getSmithNormalFormBool
  , getUnsignedDiagonal
  , findKernelInt
  , findKernelBool
  ) where

import Util
import Data.List
import Control.Parallel.Strategies

type IMatrix = [[Int]]
type BMatrix = [[Bool]]

multiply :: Num a => [[a]] -> [[a]] -> [[a]]
multiply mat1 mat2 = map (\row -> map (dotProduct row) $ transpose mat2) mat1

multiplyPar :: Num a => [[a]] -> [[a]] -> [[a]]
multiplyPar mat1 mat2 = parMap rpar (\row -> map (dotProduct row) $ transpose mat2) mat1

--insert column at one index at a different index or
--turn two columns into linear combinations of each other or
--add one column to another after multiplying by given coefficient
data ColumnOp a =
    Insert Int Int
  | Combo Int Int (a, a) (a, a)
  | Add (Int, Int) a
  | SimpleAdd (Int, Int)

applyColumnOp :: Num a => ColumnOp a -> [[a]] -> [[a]]
applyColumnOp op matrix =
  case op of
  SimpleAdd (i, j) ->
      map (\row -> (take j row) ++ (((row !! i) + (row !! j)):(drop (j + 1) row))) matrix
  Insert i j               ->
    map (\row -> (take i row) ++ (drop (i + 1) row) ++ (drop i $ take j row) ++ ((row !! i):(drop j row))) matrix
  Combo i j (a, b) (c, d) ->
    map (\row ->
      (take i row) ++ (a*(row !! i) + b*(row !! j):(drop i $ take j row)) ++ ((c*(row !! i) + d*(row !! j)):(drop (j + 1) row))) matrix
  Add (i, j) coeff          ->
    map (\row -> (take j row) ++ ((coeff*(row !! i) + (row !! j)):(drop (j + 1) row))) matrix

applyColumnOps :: Num a => [ColumnOp a] -> [[a]] -> [[a]]
applyColumnOps operations matrix =
  case operations of
  (op:ops) -> applyColumnOps ops $ applyColumnOp op matrix
  []     -> matrix

--first argument is a looping index, second argument is the upper bound for that index
--if a pivot row isn't found return nothing
--if it is found and the diagonal element is non-zero return the pivot index
--if it is found and is zero return the row index and the column index of the first non-zero element in the row
pivotHelperInt :: Int -> IMatrix -> Maybe (Either Int (Int, Int))
pivotHelperInt _ []    = error "Empty matrix passed to pivotHelper."
pivotHelperInt i (r:_) =
  if r !! i == 0 then
  case findIndex (\n -> n /= 0) r of
      Nothing -> Nothing
      Just x  -> Just $ Right (i, x)
  else if exactlyOneNonZero r then Nothing
  else Just $ Left i

pivotHelperBool :: Int -> BMatrix -> Maybe (Either Int (Int, Int))
pivotHelperBool _ []    = error "Empty matrix passed to pivotHelper."
pivotHelperBool i (r:_) =
  if not $ r !! i then
      case findIndex id r of
          Nothing -> Nothing
          Just x  -> Just $ Right (i, x)
  else if exactlyOneTrue r then Nothing
  else Just $ Left i

--calls pivot helper, rearranges the matrix if necessary, returns the matrix pair with its pivot
choosePivotInt :: Int -> IMatrix -> (Maybe Int, IMatrix)
choosePivotInt i mat =
  case pivotHelperInt i (drop i mat) of
    Nothing           -> (Nothing, mat)
    Just (Left x)     -> (Just $ mat !! x !! x, mat)
    Just (Right (x, y)) ->
      let newElems = map (switchElems x y) mat in
      (Just $ newElems !! x !! x, newElems)

--calls pivot helper, rearranges the matrix if necessary, returns the matrix pair with its pivot
choosePivotBool :: Int -> BMatrix -> (Bool, BMatrix)
choosePivotBool i mat =
  case pivotHelperBool i (drop i mat) of
    Nothing           -> (False, mat)
    Just (Left x)     -> (True, mat)
    Just (Right (x, y)) ->
      let newElems = map (switchElems x y) mat in
      (True, newElems)

colOperationHelperInt :: Int -> ((Int, Int, Int, Int, Int), Int) -> [Int] -> [Int]
colOperationHelperInt pIndex ((gcd, c1, c2, q1, q2), index) row =
  let elem1 = row !! index; elem2 = row !! pIndex in
  if index < pIndex then
  let first  = take index row
      second = drop (index + 1) (take pIndex row)
      third  = drop (pIndex + 1) row in
  first ++ ((q2*elem1 - q1*elem2) : second) ++ ((c1*elem2 + c2*elem1) : third)
  else
  let first  = take pIndex row
      second = drop (pIndex + 1) (take index row)
      third  = drop (index + 1) row in
  first ++ ((c1*elem1 + c2*elem2) : second) ++ ((q2*elem1 - q1*elem2) : third)

--does the same thing as colOperationHelper except with rows
rowOperationHelper :: Int -> ((Int, Int, Int, Int, Int), Int) -> IMatrix -> IMatrix
rowOperationHelper pIndex ((gcd, c1, c2, q1, q2), index) elems =
  let row1 = elems !! index; row2 = elems !! pIndex in
  if index < pIndex then
    let first  = take index elems
        second = drop (index + 1) (take pIndex elems)
        third  = drop (pIndex + 1) elems in
    first ++ (((q2 `mul` row1) `subtr` (q1 `mul` row2)) : second)
      ++ (((c1 `mul` row2) `add` (c2 `mul` row1)) : third)
  else
    let first  = take pIndex elems
        second = drop (pIndex + 1) (take index elems)
        third  = drop (index + 1) elems in
    first ++ (((c1 `mul` row1) `add` (c2 `mul` row2)) : second)
      ++ (((q2 `mul` row1) `subtr` (q1 `mul` row2)) : third)

rowOperationHelperPar :: Int -> ((Int, Int, Int, Int, Int), Int) -> IMatrix -> IMatrix
rowOperationHelperPar pIndex ((gcd, c1, c2, q1, q2), index) elems =
  let row1 = elems !! index; row2 = elems !! pIndex in
  if index < pIndex then
    let first  = take index elems
        second = drop (index + 1) (take pIndex elems)
        third  = drop (pIndex + 1) elems
        res    = runEval $ do
          a <- rpar $ (q2 `mul` row1) `subtr` (q1 `mul` row2)
          b <- rpar $ (c1 `mul` row2) `add` (c2 `mul` row1)
          rseq a
          rseq b
          return (a, b) in
    first ++ ((fst res):second)
    ++ ((snd res):third)
  else
    let first  = take pIndex elems
        second = drop (pIndex + 1) (take index elems)
        third  = drop (index + 1) elems
        res    = runEval $ do
          a <- rpar $ (c1 `mul` row1) `add` (c2 `mul` row2)
          b <- rpar $ (q2 `mul` row1) `subtr` (q1 `mul` row2)
          rseq a
          rseq b
          return (a, b) in
    first ++ ((fst res):second)
    ++ ((snd res):third)

--given the pivot of a matrix and the matrix itself to improve that row with column operations
--makes every element in the row divisible by the pivot
improveRowSmithInt :: Int -> (Int, IMatrix) -> IMatrix
improveRowSmithInt pIndex (pivot, elems) =
  let row          = elems !! pIndex
      nonDivis     = indexAndElem (\n -> n /= 0 && n `mod` pivot /= 0) row in
  case nonDivis of
    Nothing     -> elems
    Just (n, i) ->
      let gcdTriple    = extEucAlg pivot n
          gcd          = one gcdTriple
          transform    = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i)
          newElems     = map (colOperationHelperInt pIndex transform) elems in
      improveRowSmithInt pIndex (newElems !! pIndex !! pIndex, newElems)

improveRowSmithIntPar :: Int -> (Int, IMatrix) -> IMatrix
improveRowSmithIntPar pIndex (pivot, elems) =
    let row          = elems !! pIndex
        nonDivis     = indexAndElem (\n -> n /= 0 && n `mod` pivot /= 0) row in
    case nonDivis of
      Nothing     -> elems
      Just (n, i) ->
        let gcdTriple    = extEucAlg pivot n
            gcd          = one gcdTriple
            transform    = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i)
            newElems     = parMap rpar (colOperationHelperInt pIndex transform) elems in
        improveRowSmithInt pIndex (newElems !! pIndex !! pIndex, newElems)

--given pivot and matrix, improves pivot column with row operations
improveColInt :: Int -> (Int, IMatrix) -> IMatrix
improveColInt pIndex (pivot, elems) =
  case indexAndElem (\row -> let n = row !! pIndex in n /= 0 && n `mod` pivot /= 0) elems of
     Nothing     -> elems
     Just (r, i) ->
       let n         = r !! pIndex
           gcdTriple = extEucAlg pivot n
           gcd       = one gcdTriple
           transform = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i)
           newElems  = rowOperationHelper pIndex transform elems in
       improveColInt pIndex (newElems !! pIndex !! pIndex, newElems)

improveColIntPar :: Int -> (Int, IMatrix) -> IMatrix
improveColIntPar pIndex (pivot, elems) =
    case indexAndElem (\row -> let n = row !! pIndex in n /= 0 && n `mod` pivot /= 0) elems of
    Nothing     -> elems
    Just (r, i) ->
        let n         = r !! pIndex
            gcdTriple = extEucAlg pivot n
            gcd       = one gcdTriple
            transform = ((gcd, two gcdTriple, thr gcdTriple, n `div` gcd, pivot `div` gcd), i)
            newElems  = rowOperationHelperPar pIndex transform elems in
        improveColIntPar pIndex (newElems !! pIndex !! pIndex, newElems)

--given a matrix whose pivot row has been improved, eliminates the entries in that row
eliminateRowInt :: Int -> Int -> IMatrix -> IMatrix
eliminateRowInt pIndex pivot elems =
  let row      = elems !! pIndex
      coeffs   = map (\n -> if n == 0 then 0 else n `div` pivot) row
      newElems = map (\row -> mapWithIndex (\i elem -> if i == pIndex then elem else elem - (coeffs !! i)*(row !! pIndex)) row) elems in
  newElems

--given a matrix whose pivot column has been improved, eliminates the column
eliminateColInt :: Int -> Int -> IMatrix -> IMatrix
eliminateColInt pIndex pivot elems =
  let pRow     = elems !! pIndex
      coeffs   = map (\row -> let n = row !! pIndex in if n == 0 then 0 else n `div` pivot) elems in
  mapWithIndex (\i row -> if i == pIndex then row else row `subtr` (((coeffs) !! i) `mul` pRow)) elems

elimRowSmithBool :: Int -> BMatrix -> BMatrix
elimRowSmithBool pIndex mat =
  let row          = mat !! pIndex in
  case elemIndex True row of
    Just i  ->
      elimRowSmithBool pIndex $ applyColumnOp (SimpleAdd (pIndex, i)) mat
    Nothing -> mat

elimColBool :: Int -> BMatrix -> BMatrix
elimColBool pIndex elems =
    let pRow     = elems !! pIndex in
    mapWithIndex (\i row -> if (not $ row !! pIndex) || i == pIndex then row else row `add` pRow) elems

--does the same thing as pivotHelper except it works on columns instead of rows
columnPivotHelperInt :: Int -> IMatrix -> Maybe (Either Int (Int, Int))
columnPivotHelperInt _ []      = error "Empty matrix passed to columnPivotHelper."
columnPivotHelperInt i elems =
  let col = map (\row -> row !! i) elems in
  if elems !! i !! i == 0 then
    case findIndex (\n -> n /= 0) col of
      Nothing -> Nothing
      Just x  -> Just $ Right (i, x)
  else if exactlyOneNonZero col then Nothing
  else Just $ Left i

--calls columnPivotHelper and does necessary rearranging if pivot is found
chooseColumnPivotInt :: Int -> IMatrix -> (Maybe Int, IMatrix)
chooseColumnPivotInt index elems =
  case columnPivotHelperInt index elems of
    Nothing             -> (Nothing, elems)
    Just (Left i)       -> (Just $ elems !! i !! i, elems)
    Just (Right (i, j)) ->
      let newElems = switchElems i j elems in
      (Just $ newElems !! i !! i, newElems)

chooseColumnPivotBool :: Int -> BMatrix -> (Bool, BMatrix)
chooseColumnPivotBool index elems =
  if elems !! index !! index then (True, elems) else
    case indexAndElem (\row -> row !! index) elems of
      Nothing     -> (False, elems)
      Just (_, i) -> (True, switchElems i index elems)

getSmithNormalFormInt :: IMatrix -> IMatrix
getSmithNormalFormInt matrix =
    let maxIndex     = min (length matrix) (length $ head matrix)
        calc i m mat =
          if i > m then mat else
            case choosePivotInt i mat of
              (Nothing, mat)  ->
                case chooseColumnPivotInt i mat of
                  (Nothing, _)  -> calc (i + 1) m mat
                  (Just p, new) -> calc (i + 1) m $ (eliminateColInt i p) $ improveColInt i (p, new)
              (Just p, mat)   ->
                case chooseColumnPivotInt i $ (eliminateRowInt i p) $ improveRowSmithInt i (p, mat) of
                  (Nothing, new) -> calc (i + 1) m new
                  (Just p, new)  -> calc (i + 1) m $ (eliminateColInt i p) $ improveColInt i (p, new) in
    calc 0 maxIndex matrix

getSmithNormalFormIntPar :: IMatrix -> IMatrix
getSmithNormalFormIntPar matrix =
  let maxIndex     = min (length matrix) (length $ head matrix)
      calc i m mat =
        if i > m then mat else
          case choosePivotInt i mat of
            (Nothing, mat)  ->
              case chooseColumnPivotInt i mat of
                (Nothing, _)  -> calc (i + 1) m mat
                (Just p, new) -> calc (i + 1) m $ (eliminateColInt i p) $ improveColIntPar i (p, new)
            (Just p, mat)   ->
              case chooseColumnPivotInt i $ (eliminateRowInt i p) $ improveRowSmithIntPar i (p, mat) of
                (Nothing, new) -> calc (i + 1) m new
                (Just p, new)  -> calc (i + 1) m $ (eliminateColInt i p) $ improveColIntPar i (p, new) in
    calc 0 maxIndex matrix

getSmithNormalFormBool :: BMatrix -> BMatrix
getSmithNormalFormBool matrix =
    let maxIndex     = min (length matrix) (length $ head matrix)
        calc i m mat =
            if i > m then mat else
            case choosePivotBool i mat of
                (False, new)  ->
                  case chooseColumnPivotBool i new of
                    (False, _)  -> calc (i + 1) m mat
                    (True, new') -> calc (i + 1) m $ elimColBool i new'
                (True, new)   ->
                  case chooseColumnPivotBool i $ elimRowSmithBool i new of
                    (False, new') -> calc (i + 1) m new'
                    (True, new')  -> calc (i + 1) m $ elimColBool i new' in
    calc 0 maxIndex matrix

getUnsignedDiagonal :: Num a => [[a]] -> [a]
getUnsignedDiagonal matrix =
    map (\i -> abs $ matrix !! i !! i) [0..(min (length matrix) (length $ head matrix)) - 1]

--preps the matrix for gauss-jordan elimination 
--returns the index of the last non-zero row, the prepped matrix, and the list of column operations performed
moveAllZeroRowsBackInt :: IMatrix -> (Int, IMatrix, [ColumnOp Int])
moveAllZeroRowsBackInt elems =
  let zeroes = myfilter (\row -> forall (\n -> n == 0) row) elems
      len    = length $ head elems in
    (len - (length $ two zeroes), (thr zeroes) ++ (one zeroes), map (\i -> Insert i (len - 1)) $ two zeroes)

moveAllZeroRowsBackBool :: BMatrix -> (Int, BMatrix, [ColumnOp Bool])
moveAllZeroRowsBackBool elems =
    let zeroes = myfilter (\row -> forall not row) elems
        len    = length $ head elems in
    (len - (length $ two zeroes), (thr zeroes) ++ (one zeroes), map (\i -> Insert i (len - 1)) $ two zeroes)

--makes every element after the pivot in the pivot row divisible by the pivot
improveRowGaussInt :: Int -> (Int, IMatrix) -> ([ColumnOp Int], IMatrix)
improveRowGaussInt pIndex (pivot, elems) =
  let improve ops list mat =
        case list of
          []          -> (ops, mat) 
          ((n, i):xs) ->
            let gcdTriple    = extEucAlg pivot n
                gcd          = one gcdTriple
                transform    = ((gcd, two gcdTriple, thr gcdTriple, pivot `div` gcd, n `div` gcd), i)
                newElems     = map (colOperationHelperInt pIndex transform) mat in
            improve (ops ++ [Combo pIndex i (two gcdTriple, thr gcdTriple) (five4 $ fst transform, five5 $ fst transform)]) xs newElems in
  improve [] (filter (\pair -> snd pair > pIndex) $ indexAndElems (\n -> n /= 0 && n `mod` pivot /= 0) (elems !! pIndex)) elems

improveRowGaussBool :: Int -> BMatrix -> ([ColumnOp Bool], BMatrix)
improveRowGaussBool pIndex elems =
  let improve ops list mat =
        case list of
          []          -> (ops, mat) 
          (i:is)      ->
            let op = SimpleAdd (i, pIndex) in
            improve (ops ++ [op]) is $ applyColumnOp op mat in
  improve [] (elemIndices True $ elems !! pIndex) elems  

--eliminates all the entries in the pivot row that ome after the pivot, after the matrix has been improved
eliminateEntriesGaussInt :: Int -> Int -> IMatrix -> ([ColumnOp Int], IMatrix)
eliminateEntriesGaussInt pIndex pivot elems =
  let row            = elems !! pIndex
      len            = length $ head elems
      elim ops i mat =
        if i == len then (ops, mat)
        else let coeff = (row !! i) `div` pivot in
             elim (ops ++ [Add (pIndex, i) coeff]) (i + 1) $
               map (\r -> (take i r) ++ (((r !! i) - coeff*(r !! pIndex)):(drop (i + 1) r))) mat in
  elim [] (pIndex + 1) elems

eliminateEntriesGaussBool :: Int -> BMatrix -> ([ColumnOp Bool], BMatrix)
eliminateEntriesGaussBool pIndex elems =
  let row            = elems !! pIndex
      len            = length $ head elems
      elim ops i mat =
          if i == len then (ops, mat)
          else let op = SimpleAdd (pIndex, i) in
            elim (ops ++ [op]) (i + 1) $ applyColumnOp op mat in
  elim [] (pIndex + 1) elems

--finds the basis of the kernel of a matrix, arranges basis vectors into the rows of a matrix
findKernelInt :: IMatrix -> IMatrix
findKernelInt matrix =
  let len      = length matrix
      len0     = length $ head matrix
      maxIndex = if len > len0 then len0 else len
      zeroData = moveAllZeroRowsBackInt matrix
      doColOps elems index opList =
        if index == (one zeroData) || index == maxIndex then (opList, elems) else 
          case choosePivotInt index elems of --Make sure operations done by choosePivot are taken into account when transforming identity
            (Nothing, m) -> error "findKernelInt.doColOps ran into zero row"
            (Just p, mx) ->
              let improved = improveRowGaussInt index (p, mx)
                  elim     = eliminateEntriesGaussInt index p $ snd improved in
              doColOps (snd elim) (index + 1) (opList ++ (fst improved) ++ (fst elim))
      gaussElim   = doColOps (two zeroData) 0 []
      identity    = applyColumnOps (thr zeroData ++ fst gaussElim) $
        map (\i -> (replicate i 0) ++ (1:(replicate (len0 - i) 0))) [0..len - 1] --identity matrix
      elems       = snd gaussElim in
  map (\i -> map (\row -> row !! i) identity) $ filter (\i -> forall (\row -> row !! i == 0) elems) [0..len - 1]

findKernelBool :: BMatrix -> BMatrix
findKernelBool matrix =
  let len      = length matrix
      len0     = length $ head matrix
      maxIndex = if len > len0 then len0 else len
      zeroData = moveAllZeroRowsBackBool matrix
      doColOps elems index opList =
        if index == (one zeroData) || index == maxIndex then (opList, elems) else 
          case choosePivotBool index elems of
            (False, m) -> error "findKernelBool.doColOps ran into zero row"
            (True, mx) ->
              let improved = improveRowGaussBool index mx
                  elim     = eliminateEntriesGaussBool index $ snd improved in
              doColOps (snd elim) (index + 1) (opList ++ (fst improved) ++ (fst elim))
      gaussElim   = doColOps (two zeroData) 0 []
      identity    = applyColumnOps (thr zeroData ++ fst gaussElim) $
        map (\i -> (replicate i False) ++ (True:(replicate (len0 - i) False))) [0..len - 1]
      elems       = snd gaussElim in
  map (\i -> map (\row -> row !! i) identity) $ filter (\i -> forall (\row -> row !! i == 0) elems) [0..len - 1]