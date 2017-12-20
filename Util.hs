--many of the things in this file aren't used
module Util where

import Data.List

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
flatten (x:xs) = x ++ (flatten xs)

mul :: Num a => a -> [a] -> [a]
mul s = map (*s)

add :: Num a => [a] -> [a] -> [a]
add [] []         = []
add [] y          = y
add x []          = x
add (x:xs) (y:ys) = (x + y) : (xs `add` ys)

subtr :: Num a => [a] -> [a] -> [a]
subtr [] []         = []
subtr [] y          = y
subtr x []          = x
subtr (x:xs) (y:ys) = (x - y) : (xs `subtr` ys)

dotProduct :: Num a => [a] -> [a] -> a
dotProduct [] _          = error "Second vector too big"
dotProduct _ []          = error "First vector too big"
dotProduct (x:xs) (y:ys) = x*y + dotProduct xs ys

switchElems :: Int -> Int -> [a] -> [a]
switchElems i j list
  | j == i              = list
  | j < i               =
    let first  = take j list
        second = drop (j + 1) (take i list)
        third  = drop (i + 1) list in
    first ++ ((list !! i) : second) ++ ((list !! j) : third)
  | otherwise           =
    let first  = take i list
        second = drop (i + 1) (take j list)
        third  = drop (j + 1) list in
    first ++ ((list !! j) : second) ++ ((list !! i) : third)

switchConsecutive :: Int -> [a] -> [a]
switchConsecutive i list =
  (take i list) ++ ((list !! (i + 1)):(list !! i):(drop (i + 2) list))

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
forall _ []     = True
forall p (x:xs) = (p x) && (forall p xs)

exists :: Eq a => a -> [a] -> Bool
exists elem list =
  case list of
    []       -> False
    (x : xs) -> (x == elem) || (exists elem xs)

existsPredicate :: (a -> Bool) -> [a] -> Bool
existsPredicate p []     = False
existsPredicate p (x:xs) =
  if p x then True
  else existsPredicate p xs

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f list =
  let helper _ []     = []
      helper n (x:xs) = (f n x):(helper (n + 1) xs) in
  helper 0 list

filterWithIndex :: (Int -> a -> Bool) -> [a] -> [a]
filterWithIndex p list =
  let helper = \i l ->
        case l of
          []     -> []
          (x:xs) ->
            if p i x  then x : (helper (i + 1) xs)
            else helper (i + 1) xs in
  helper 0 list

indexAndElem :: (a -> Bool) -> [a] -> Maybe (a, Int)
indexAndElem p list =
  let helper _ []     = Nothing
      helper i (x:xs) =
        if p x then Just (x, i)
        else helper (i + 1) xs in
  helper 0 list

indexAndElems :: (a -> Bool) -> [a] -> [(a, Int)]
indexAndElems p list =
  let helper i l =
        case l of
          (x:xs) ->
            if p x then (x, i) : (helper (i + 1) xs)
            else helper (i + 1) xs
          []     -> [] in
  helper 0 list

exactlyOneNonZero :: (Eq a, Num a) => [a] -> Bool
exactlyOneNonZero list =
  let helper b []     = b
      helper b (x:xs) =
        if x /= fromIntegral 0 then
          if b then False
          else helper True xs
        else helper b xs in
  helper False list

exactlyOneTrue :: [Bool] -> Bool
exactlyOneTrue list =
  let helper b []     = b
      helper b (x:xs) =
        if x then
          if b then False
          else helper True xs
        else helper b xs in
  helper False list

--if the lists differ by one element, returns that element
--otherwise returns nothing
diffByOneElem :: Eq a => [a] -> [a] -> Maybe a
diffByOneElem list1 list2 =
  let helper a [] []         = a
      helper a (x:xs) (y:ys) =
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
levenshtein s1 s2 = last $ foldl transform [0 .. length s1] s2
  where
    transform ns@(n:ns1) c = scanl calc (n + 1) $ zip3 s1 ns ns1
      where
        calc z (c1, x, y) = minimum [y + 1, z + 1, x + fromEnum (c1 /= c)]

--finds the element of the first list that is missing in the second
findMissing :: Eq a => [a] -> [a] -> Maybe a
findMissing (x:xs) sup =
  case elemIndex x sup of
    Nothing -> Just x
    Just _  -> findMissing xs sup
findMissing [] sup     = Nothing

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

minMax :: Ord a => a -> a -> (a, a)
minMax a b =
  if a > b then (b, a)
  else (a, b)