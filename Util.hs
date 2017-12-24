--TODO: investigate parallel mapping over vectors
module Util where

import Data.List as L
import Data.Vector as V
import Control.Parallel.Strategies

one (a, _, _) = a
two (_, b, _) = b
thr (_, _, c) = c

not1 (_, b, c) = (b, c)
not2 (a, _, c) = (a, c)
not3 (a, b, _) = (a, b)

five1 (a, _, _, _, _) = a
five2 (_, b, _, _, _) = b
five3 (_, _, c, _, _) = c
five4 (_, _, _, d, _) = d
five5 (_, _, _, _, e) = e

four1 (a, _, _, _) = a
four2 (_, b, _, _) = b
four3 (_, _, c, _) = c
four4 (_, _, _, d) = d

flatten :: [[a]] -> [a]
flatten []     = []
flatten (x:xs) = x L.++ (flatten xs)

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

switchConsecutive :: Int -> [a] -> [a]
switchConsecutive i list =
  (L.take i list) L.++ ((list !! (i + 1)):(list !! i):(L.drop (i + 2) list))

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
            eeaHelper (r2, nextr) (s2, nexts) (t2, nextt) in
  eeaHelper (a, b) (0, 1) (1, 0)

forall :: (a -> Bool) -> [a] -> Bool
forall p (x:xs) = (p x) && (forall p xs)
forall _ []     = True

forallVec :: (a -> Bool) -> Vector a -> Bool
forallVec p vector = 
  if V.null vector then True
  else (p $ V.head vector) && (forallVec p $ V.tail vector)

exists :: Eq a => a -> [a] -> Bool
exists elem list =
  case list of
    []       -> False
    (x : xs) -> (x == elem) || (exists elem xs)

existsVec :: Eq a => a -> Vector a -> Bool
existsVec elem vector =
  if V.null vector then False
  else let x = V.head vector; xs = V.tail vector in
    (x == elem) || (existsVec elem xs)

existsPredicate :: (a -> Bool) -> [a] -> Bool
existsPredicate p []     = False
existsPredicate p (x:xs) =
  if p x then True
  else existsPredicate p xs

mapWithIndex :: (Int -> a -> b) -> Vector a -> Vector b
mapWithIndex f vector =
  let helper i vec =
        if V.null vec then empty
        else cons (f i $ V.head vec) $ helper (i + 1) (V.tail vec) in
  helper 0 vector

filterWithIndex :: (Int -> a -> Bool) -> [a] -> [a]
filterWithIndex p list =
  let helper = \i l ->
        case l of
          []     -> []
          (x:xs) ->
            if p i x  then x : (helper (i + 1) xs)
            else helper (i + 1) xs in
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

--from rosetta code
levenshtein :: String -> String -> Int
levenshtein s1 s2 = L.last $ L.foldl transform [0 .. L.length s1] s2
  where
    transform ns@(n:ns1) c = L.scanl calc (n + 1) $ L.zip3 s1 ns ns1
      where
        calc z (c1, x, y) = L.minimum [y + 1, z + 1, x + fromEnum (c1 /= c)]

--finds the element of the first list that is missing in the second
findMissing :: Eq a => Vector a -> Vector a -> Maybe a
findMissing vec1 vec2 =
  if V.null vec1 then Nothing
  else let x = V.head vec1; xs = V.tail vec1 in
    case V.elemIndex x vec2 of
      Nothing -> Just x
      Just _  -> findMissing xs vec2

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

--first list satisfies predicate,
--second is the list of indices that satisfy the predicate
--third list does not satisfy the predicate
myfilter :: (a -> Bool) -> [a] -> ([a], [Int], [a])
myfilter p list =
  let helper = \i l ->
        case l of
          []     -> ([],[],[])
          (x:xs) ->
            let rest = helper (i + 1) xs in
            if p x then (x:(one rest), i:(two rest), thr rest)
            else (one rest, two rest, x:(thr rest)) in
  helper 0 list

myfilterVec :: (a -> Bool) -> Vector a -> (Vector a, Vector Int, Vector a)
myfilterVec p vector =
  let helper = \i v ->
        if V.null v then (empty, empty, empty)
        else let x = V.head v; xs = V.tail v in
          let rest = helper (i + 1) xs in
          if p x then (cons x (one rest), cons i (two rest), thr rest)
          else (one rest, two rest, cons x (thr rest)) in
  helper 0 vector

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

vectorFromList :: [a] -> Vector a
vectorFromList (x:xs) = cons x $ vectorFromList xs
vectorFromList []     = empty

listFromVector :: Vector a -> [a]
listFromVector vector =
  if V.null vector then []
  else (V.head vector):(listFromVector $ V.tail vector)

moveToBack :: [Int] -> Vector a -> Vector a
moveToBack (i:is) vector =
  moveToBack is $ ((V.take i vector) V.++ (V.drop (i + 1) vector)) `snoc` (vector ! i)
moveToBack [] vector     = vector

parMapVec :: (a -> b) -> Vector a -> Vector b
parMapVec f v = runEval $ evalTraversable rpar $ V.map f v

range :: Int -> Int -> Vector Int
range x y
  | x == y = x `cons` empty
  | x < y  = x `cons` (range (x + 1) y)
  | x > y  = (range x (y + 1)) `snoc` y