--TODO: investigate parallel mapping over vectors
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
                nextt = fst t - q*t2 in
            eeaHelper (r2, nextr) (s2, nexts) (t2, nextt)
      gcdTriple = eeaHelper (a, b) (0, 1) (1, 0) in
  if one gcdTriple < 0 then (-(one gcdTriple), -(two gcdTriple), -(thr gcdTriple))
  else gcdTriple

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
subtr vec1 vec2 =
  if (V.null vec1) && (V.null vec2) then empty
  else cons ((V.head vec1) - (V.head vec2)) ((V.tail vec1) `subtr` (V.tail vec2))

dotProduct :: Num a => Vector a -> Vector a -> a
dotProduct vec1 vec2 = V.sum $ V.zipWith (*) vec1 vec2

switchElems :: Int -> Int -> Vector a -> Vector a
switchElems i j vector
  | j == i              = vector
  | j < i               =
    let first  = V.take j vector
        second = V.drop (j + 1) (V.take i vector)
        third  = V.drop (i + 1) vector in
    first V.++ (cons (vector ! i) second) V.++ (cons (vector ! j) third)
  | otherwise           =
    let first  = V.take i vector
        second = V.drop (i + 1) (V.take j vector)
        third  = V.drop (j + 1) vector in
    first V.++ (cons (vector ! j) second) V.++ (cons (vector ! i) third)

getCombos :: Vector a -> Vector (Vector a)
getCombos vector =
  let len    = V.length vector
      calc i =
        if i == len then empty
        else let i1 = i + 1 in
          ((V.take i vector) V.++ (V.drop i1 vector)) `cons` (calc i1) in
  calc 0

forallVec :: (a -> Bool) -> Vector a -> Bool
forallVec p vector = 
  if V.null vector then True
  else (p $ V.head vector) && (forallVec p $ V.tail vector)

mapWithIndex :: (Int -> a -> b) -> Vector a -> Vector b
mapWithIndex f vector =
  let helper i vec =
        if V.null vec then empty
        else cons (f i $ V.head vec) $ helper (i + 1) (V.tail vec) in
  helper 0 vector

parMapWithIndex :: (Int -> a -> b) -> Vector a -> Vector b
parMapWithIndex f vector =
  let helper i vec = runEval $
        if V.null vec then return empty
        else
          let current = f i $ V.head vec; rest = helper (i + 1) $ V.tail vec in
          rpar current >> rseq rest >> (return $ current `cons` rest) in
  helper 0 vector

parMapWithIndexList :: (Int -> a -> b) -> [a] -> Vector b
parMapWithIndexList f list =
  let helper i l = runEval $
        if L.null l then return empty
        else
          let current = f i $ L.head l; rest = helper (i + 1) $ L.tail l in
          rpar current >> rseq rest
              >> (return $ current `cons` rest) in
  helper 0 list

elemAndIndex :: (a -> Bool) -> Vector a -> Maybe (a, Int)
elemAndIndex p vector =
  let helper i vec
        | V.null vec     = Nothing
        | p $ V.head vec = Just (V.head vec, i)
        | otherwise      = helper (i + 1) $ V.tail vec in
  helper 0 vector

elemAndIndices :: (a -> Bool) -> Vector a -> [(a, Int)]
elemAndIndices p vector =
  let helper i vec
        | V.null vec     = []
        | p $ V.head vec = (V.head vec, i) : (helper (i + 1) $ V.tail vec)
        | otherwise      = helper (i + 1) $ V.tail vec in
  helper 0 vector

exactlyOneNonZero :: (Eq a, Num a) => Vector a -> Bool
exactlyOneNonZero vector =
  let helper b vec =
        if V.null vec then b else
        if V.head vec /= fromIntegral 0 then
          if b then False
          else helper True $ V.tail vec
        else helper b $ V.tail vec in
  helper False vector

exactlyOneTrue :: Vector Bool -> Bool
exactlyOneTrue vector =
  let helper b vec =
        if V.null vec then b else
        if V.head vec then
          if b then False
          else helper True $ V.tail vec
        else helper b $ V.tail vec in
  helper False vector

--if the lists differ by one element, returns that element
--otherwise returns nothing
diffByOneElem :: Eq a => Vector a -> Vector a -> Maybe a
diffByOneElem list1 list2 =
  let helper a v1 v2 =
        if V.null v1 || V.null v2 then Nothing
        else let x = V.head v1; y = V.head v2; xs = V.tail v1; ys = V.tail v2 in
          case a of
            Just z  ->
              if x == y then Nothing
              else helper a xs ys
            Nothing ->
              if x == y then helper (Just x) xs ys
              else helper (Just y) xs ys in
  helper Nothing list1 list2

--finds the element of the first list that is missing in the second
findMissing :: Eq a => Vector a -> Vector a -> Maybe a
findMissing vec1 vec2 =
  if V.null vec1 then Nothing
  else let x = V.head vec1 in
    case V.elemIndex x vec2 of
      Nothing -> Just x
      Just _  -> findMissing (V.tail vec1) vec2

--returns number of elements satisying the predicate and a list of the elements
filterAndCount :: (a -> Bool) -> [a] -> (Int, [a])
filterAndCount p list =
  let helper = \arg i result ->
       case arg of
         []     -> (i, result)
         (x:xs) ->
           if p x then helper xs i (x:result)
           else helper xs (i + 1) result in
  helper list 0 []

myfilterVec :: (a -> Bool) -> Vector a -> (Vector a, Vector Int, Vector a)
myfilterVec p vector =
  let helper = \i v ->
        if V.null v then (empty, empty, empty)
        else let x = V.head v; xs = V.tail v in
          let rest = helper (i + 1) xs in
          if p x then (cons x (one rest), cons i (two rest), thr rest)
          else (one rest, two rest, cons x (thr rest)) in
  helper 0 vector

--orders a list of vectors from greatest to least length
sortVecs :: [Vector a] -> [Vector a]
sortVecs []     = []
sortVecs (v:vs) =
  let len  = V.length v
      less = sortVecs $ L.filter (\u -> V.length u < len) vs
      more = sortVecs $ L.filter (\u -> V.length u >= len) vs in
  more L.++ [v] L.++ less

parMapVec :: (a -> b) -> Vector a -> Vector b
parMapVec f v = runEval $ evalTraversable rpar $ V.map f v

range :: Int -> Int -> Vector Int
range x y
  | x == y = x `cons` empty
  | x < y  = x `cons` (range (x + 1) y)
  | x > y  = (range x (y + 1)) `snoc` y

vecConcat :: Vector (Vector a) -> Vector a
vecConcat v =
  if V.null v then empty
  else V.head v V.++ (vecConcat $ V.tail v)

--union of two vectors
(##) :: Eq a => Vector a -> Vector a -> Vector a
a ## b =
  let check u v =
        if V.null u then v
        else let h = V.head u in
          if V.elem h v then check (V.tail u) v
          else h `cons` (check (V.tail u) v) in
  if V.null b then a
  else check a b

--union of many vectors
bigU :: Eq a => Vector (Vector a) -> Vector a
bigU = V.foldl1 (##)

replaceElem :: Int -> a -> [a] -> [a]
replaceElem i e l = (L.take i l) L.++ (e:(L.drop (i + 1) l))

evalPar :: a -> [a] -> [a]
evalPar c r = runEval $ rpar c >> rseq r >> return (c:r)