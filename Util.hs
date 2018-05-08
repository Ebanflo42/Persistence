module Util where
import Data.List as L
import Data.Vector as V
import Control.Parallel.Strategies

xor :: Bool -> Bool -> Bool
xor False False = False
xor True False  = True
xor False True  = True
xor True True   = False

instance Num Bool where
  p + q  = p `xor` q
  p * q  = p && q
  p - q  = p `xor` (not q)
  negate = not
  abs    = id
  fromInteger 0 = False
  fromInteger _ = True
  signum bool   = if bool then 1 else 0

one (a, _, _) = a
two (_, b, _) = b
thr (_, _, c) = c
not1 (_, b, c) = (b, c)
not2 (a, _, c) = (a, c)
not3 (a, b, _) = (a, b)

flatten :: Vector (Vector a) -> Vector a
flatten = V.foldl1 (V.++)

mul :: Num a => a -> Vector a -> Vector a
mul s = V.map (*s)

add :: Num a => Vector a -> Vector a -> Vector a
add = V.zipWith (+)

subtr :: Num a => Vector a -> Vector a -> Vector a
subtr = V.zipWith (\x y -> x - y)

dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct vec1 vec2
  | a && b = fromIntegral 0
  | a      = error "First vector passed to dotProduct too short."
  | b      = error "Second vector passed to dotProduct too short."
  | otherwise   = (V.head vec1)*(V.head vec2) + (dotProduct (V.tail vec1) (V.tail vec2))
    where a = V.null vec1; b = V.null vec2

--extended Euclidean algorithm
extEucAlg :: Integral a => a -> a -> (a, a, a)
extEucAlg a b =
  let eeaHelper r s t =
        case snd r of
          0 -> (fst r, fst s, fst t)
          _ ->
            let r1    = fst r
                r2    = snd r
                s2    = snd s
                t2    = snd t
                q     = r1 `div` r2
                nextr = r1 - q*r2
                nexts = fst s - q*s2
                nextt = fst t - q*t2
            in eeaHelper (r2, nextr) (s2, nexts) (t2, nextt)
  in (\(x, y, z) -> if x < 0 then (-x, -y, -z) else (x, y, z)) $ eeaHelper (a, b) (0, 1) (1, 0)

divides :: Int -> Int -> Bool
0 `divides` b = False
a `divides` b
  | b < 0     = False
  | b == 0    = True
  | otherwise = a `divides` (b - (abs a))

switchElems ::Int -> Int -> Vector a -> Vector a
switchElems i j vector
  | j == i    = vector
  | j < i     =
    let first  = V.take j vector
        second = V.drop (j + 1) (V.take i vector)
        third  = V.drop (i + 1) vector
    in first V.++ (cons (vector ! i) second) V.++ (cons (vector ! j) third)
  | otherwise =
    let first  = V.take i vector
        second = V.drop (i + 1) (V.take j vector)
        third  = V.drop (j + 1) vector
    in first V.++ (cons (vector ! j) second) V.++ (cons (vector ! i) third)

--all arrays missing one element from the original array
getCombos :: Vector a -> Vector (Vector a)
getCombos vector =
  let len    = V.length vector
      calc i =
        if i == len then empty
        else
          let i1 = i + 1
          in ((V.take i vector) V.++ (V.drop i1 vector)) `cons` (calc i1)
  in calc 0

concatVec :: Vector (Vector a) -> Vector a
concatVec = V.foldl1 (V.++)

forallVec :: (a -> Bool) -> Vector a -> Bool
forallVec p vector =
  if V.null vector then True
  else (p $ V.head vector) && (forallVec p $ V.tail vector)

mapWithIndex :: (Int -> a -> b) -> Vector a -> Vector b
mapWithIndex f vector =
  let helper i vec =
        if V.null vec then empty
        else cons (f i $ V.head vec) $ helper (i + 1) (V.tail vec)
  in helper 0 vector

parMapWithIndex :: (Int -> a -> b) -> Vector a -> Vector b
parMapWithIndex f vector =
  let helper i vec = runEval $
        if V.null vec then return empty
        else
          let current = f i $ V.head vec; rest = helper (i + 1) $ V.tail vec
          in rpar current >> rseq rest >> (return $ current `cons` rest)
  in helper 0 vector

parMapWithIndexList :: (Int -> a -> b) -> [a] -> Vector b
parMapWithIndexList f list =
  let helper i l = runEval $
        if L.null l then return empty
        else
          let current = f i $ L.head l; rest = helper (i + 1) $ L.tail l
          in rpar current >> rseq rest
            >> (return $ current `cons` rest)
  in helper 0 list

elemAndIndex :: (a -> Bool) -> Vector a -> Maybe (a, Int)
elemAndIndex p vector =
  let helper i vec
        | V.null vec     = Nothing
        | p $ V.head vec = Just (V.head vec, i)
        | otherwise      = helper (i + 1) $ V.tail vec
  in helper 0 vector

elemAndIndices :: (a -> Bool) -> Vector a -> [(a, Int)]
elemAndIndices p vector =
  let helper i vec
        | V.null vec     = []
        | p $ V.head vec = (V.head vec, i) : (helper (i + 1) $ V.tail vec)
        | otherwise      = helper (i + 1) $ V.tail vec
  in helper 0 vector

findBothElems :: (a -> b -> Bool) -> Vector a -> Vector b -> Vector (a, b)
findBothElems rel vector1 vector2 =
  let calc i result =
        let a = vector1 ! i
        in case V.find (\b -> rel a b) vector2 of
          Just b  -> calc (i + 1) $ result `snoc` (a, b)
          Nothing -> calc (i + 1) result
  in calc 0 V.empty

--fst vector satisfies predicate, snd vector does not
biFilter :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
biFilter p vector =
  let calc true false v
        | V.null v  = (true, false)
        | p x       = calc (true `snoc` x) false $ V.tail v
        | otherwise = calc true (false `snoc` x) $ V.tail v
        where x = V.head v
  in calc V.empty V.empty vector

--orders a list of vectors from greatest to least length
sortVecs :: [Vector a] -> [Vector a]
sortVecs []     = []
sortVecs (v:vs) =
  let len  = V.length v
      less = sortVecs $ L.filter (\u -> V.length u < len) vs
      more = sortVecs $ L.filter (\u -> V.length u >= len) vs
  in more L.++ [v] L.++ less

parMapVec :: (a -> b) -> Vector a -> Vector b
parMapVec f v = runEval $ evalTraversable rpar $ V.map f v

filterMap :: (a -> a -> b) -> (b -> Bool) -> a -> [a] -> Vector (a, a, b)
filterMap f p x = V.fromList . L.filter (\(_, _, b) -> p b) . L.map (\y -> (x, y, f x y))

filterWithIndex :: (Int -> a -> Bool) -> Vector a -> Vector a
filterWithIndex p vector =
  let maxIndex = V.length vector - 1
      calc i
        | i == maxIndex    = V.empty
        | p i (vector ! i) = (vector ! i) `cons` calc (i + 1)
        | otherwise        = calc (i + 1)
  in calc 0

range :: Int -> Int -> Vector Int
range x y
  | x == y = x `cons` empty
  | x < y  = x `cons` (range (x + 1) y)
  | x > y  = (range x (y + 1)) `snoc` y

vecConcat :: Vector (Vector a) -> Vector a
vecConcat v =
  if V.null v then empty
  else V.head v V.++ (vecConcat $ V.tail v)

replaceElem :: Int -> a -> Vector a -> Vector a
replaceElem i e v = (V.take i v) V.++ (e `cons` (V.drop (i + 1) v))

quicksort :: (a -> a -> Bool) -> Vector a -> Vector a
quicksort rel vector = --rel is the > operator
  if V.null vector then empty
  else
    let x       = V.head vector
        xs      = V.tail vector
        lesser  = V.filter (rel x) xs
        greater = V.filter (not . (rel x)) xs
    in (quicksort rel lesser) V.++ (x `cons` (quicksort rel greater))

bigU :: Eq a => Vector (Vector a) -> Vector a
bigU =
  let exists x v
        | V.null v      = False
        | V.head v == x = True
        | otherwise     = exists x (V.tail v)
      union v1 v2 =
        if V.null v1 then v2
        else
          let x = V.head v1
          in
            if exists x v2 then union (V.tail v1) v2
            else union (V.tail v1) (x `cons` v2)
  in V.foldl1 union

--list, value, low, high, return int
binarySearch :: Ord a => a -> Vector a -> Int -> Int -> Maybe Int
binarySearch value xs low high
  | high < low        = Nothing
  | xs ! mid > value  = binarySearch value xs low (mid - 1)
  | xs ! mid < value  = binarySearch value xs (mid + 1) high
  | otherwise         = Just mid
  where mid = low + ((high - low) `div` 2)

--intersection of ordered vectors
(|^|) :: Ord a => Vector a -> Vector a -> Vector a
vector1 |^| vector2 =
  let len          = V.length vector2 - 1
      calc acc v =
        if V.null v then acc
        else
          let x = V.head v; xs = V.tail v
          in case binarySearch x vector2 0 len of
            Just _  -> calc (x `cons` acc) xs
            Nothing -> calc acc xs
  in calc V.empty vector1

existsVec :: (a -> Bool) -> Vector a -> Bool
existsVec p v
  | V.null v     = False
  | p $ V.head v = True
  | otherwise    = existsVec p $ V.tail v

findElems :: (a -> Bool) -> Vector a -> Vector a
findElems p v = V.map ((!) v) $ V.findIndices p v

--if the relation is a "greater than" operator, this would find the minimum of the vector
foldRelation :: (a -> a -> Bool) -> Vector a -> a
foldRelation rel vec =
  let calc w v
        | V.null v  = w
        | rel w x   = calc x xs
        | otherwise = calc w xs
        where x = V.head v; xs = V.tail v
  in calc (V.head vec) (V.tail vec)

elemIndexUnsafe :: Eq a => a -> Vector a -> Int
elemIndexUnsafe elem vector =
  let find i v
        | V.null v         = error "Element isn't here, Util.elemIndexUnsafe"
        | V.head v == elem = i
        | otherwise        = find (i + 1) $ V.tail v
  in find 0 vector

reverseFind :: (a -> Bool) -> Vector a -> Maybe (a, Int)
reverseFind p vector =
  let find i
        | i == (-1)      = Nothing
        | p $ vector ! i = Just (vector ! i, i)
        | otherwise      = find $ i - 1
  in find $ (V.length vector) - 1

replaceElemList :: Int -> a -> [a] -> [a]
replaceElemList i e l = (L.take i l) L.++ (e:(L.drop (i + 1) l))

evalPar :: a -> [a] -> [a]
evalPar c r = runEval $ rpar c >> rseq r >> return (c:r)

subscript :: Int -> String
subscript = (\c -> c : "") . (!!) "₀₁₂₃₄₅₆₇₈₉"

supscript :: Int -> String
supscript i =
  let str  = "⁰¹²³⁴⁵⁶⁷⁸⁹"
      f s  =
        let i10  = i `div` 10
            im10 = i `mod` 10
        in
          if i10 == 0 then (str !! im10):[]
          else (str !! im10):(supscript i10)
  in L.reverse $ f str