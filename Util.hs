module Util where

import Data.List
import Control.Parallel

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

dotProduct :: Num a => [a] -> [a] -> a
dotProduct [] _          = error "Second vector too big"
dotProduct _ []          = error "First vector too big"
dotProduct (x:xs) (y:ys) = x*y + dotProduct xs ys

--first argument is current index, third argument is the two indices to split at, second argument is which index is greater,
--fourth arg is the list to be split
getSubLists :: Int -> (Int, Int) -> [a] -> ([a], [a], [a])
getSubLists _ _ []          = ([], [], [])
getSubLists i (a, b) (x:xs) =
  if i < a then let sublists = getSubLists (i + 1) (a, b) xs in
    (x : (one sublists), two sublists, thr sublists)
  else if i < b then let sublists = getSubLists (i + 1) (a, b) xs in
    (one sublists, x : (two sublists), thr sublists)
  else let sublists = getSubLists (i + 1) (a, b) xs in
    (one sublists, two sublists, x : (thr sublists))

splitListHelper :: Int -> ([a], Maybe a, [a], Maybe a, [a]) -> Int -> Int -> [a] -> ([a], Maybe a, [a], Maybe a, [a])
splitListHelper _ result _ _ []  = result
splitListHelper current (l1, e1, l2, e2, l3) i j (x:xs)
  | current < i  = splitListHelper (current + 1) (x:l1, e1, l2, e2, l3) i j xs
  | current == i = splitListHelper (current + 1) (l1, Just x, l2, e2, l3) i j xs
  | current < j  = splitListHelper (current + 1) (l1, e1, x:l2, e2, l3) i j xs
  | current == j = splitListHelper (current + 1) (l1, e1, l2, Just x, l3) i j xs
  | otherwise    = splitListHelper (current + 1) (l1, e1, l2, e2, x:l3) i j xs

switchElems :: Int -> Int -> [a] -> [a]
{--
switchElems i j list =
  if i == j then list
  else let fstTwo = splitAt i list
           sndTwo = splitAt (j - i) (snd fstTwo) in
       (fst fstTwo) ++ ((head $ snd sndTwo) : (tail $ fst sndTwo)) ++ ((head $ fst sndTwo) : (tail $ snd sndTwo))
--}
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

--extended Euclidean algorithm
extEucAlg :: Integral a => a -> a -> (a, a, a)
extEucAlg a b = --eeaHelper (a, b) (0, 1) (1, 0)
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
            eeaHelper (par (s2, nexts) (r2, nextr)) (par (t2, nextt) (s2, nexts)) (t2, nextt) in
    eeaHelper (a, b) (0, 1) (1, 0)    

--elimates duplicates in first argument, second argument is the result
regroupElems :: Eq a => [[a]] -> [[a]] -> [[a]]
regroupElems arg res =
    case arg of
        []     -> res
        (x:xs) ->
          if exists x res then regroupElems xs res
          else regroupElems xs (x : res)

collect :: Eq a => [[[a]]] -> [[a]]
collect block =
  let helper result arg =
        case arg of
          []     -> result
          (x:xs) -> helper (regroupElems x result) xs in
  helper [] block

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
{--
parMap :: (a -> b) -> [a] -> [b]
parMap f []        = []
parMap f [x]       = [f x]
parMap f (x:x':xs) =
  let rest = parMap f xs in
  par rest ((f x):(f x'):rest)
--}
parMap :: (a -> b) -> [a] -> [b]
parMap f [] = []
parMap f (x:xs) =
  let rest = parMap f xs in
  par rest ((f x):rest)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f list =
  let helper _ []     = []
      helper n (x:xs) = (f n x):(helper (n + 1) xs) in
  helper 0 list

parMapWithIndex :: (Int -> a -> b) -> [a] -> [b]
parMapWithIndex f list =
  let helper _ []        = []
      helper i (x:xs) =
        let rest = helper (i + 1) xs in
        par rest ((f i x):rest) in
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

exactlyOneNonZero :: (Eq a, Num a) => [a] -> Bool
exactlyOneNonZero list =
  let helper b []     = b
      helper b (x:xs) =
        if x /= fromIntegral 0 then
          if b then False
          else helper True xs
        else helper b xs in
  helper False list

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
  
levenshtein :: String -> String -> Int
levenshtein s1 s2 = last $ foldl transform [0 .. length s1] s2
  where
    transform ns@(n:ns1) c = scanl calc (n + 1) $ zip3 s1 ns ns1
      where
        calc z (c1, x, y) = minimum [y + 1, z + 1, x + fromEnum (c1 /= c)]

findMissing :: Eq a => [a] -> [a] -> a
findMissing (x:xs) sup =
  case elemIndex x sup of
    Nothing -> x
    Just _  -> findMissing xs sup

minusOnePow :: Integral a => a -> a
minusOnePow x =
  case x `mod` 2 of
    0 -> 1
    1 -> -1

filterAndCount :: (a -> Bool) -> [a] -> (Int, [a])
filterAndCount p list =
  let helper = \arg i result ->
       case arg of
         []     -> (i, result)
         (x:xs) ->
           if p x then helper xs i (x:result)
           else helper xs (i + 1) result in
  helper list 0 []

--first list satisfies predicate, second does not
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