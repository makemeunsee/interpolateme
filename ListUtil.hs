module ListUtil where


import Data.List (partition)


-- chop down a list into sublists of size n
chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)


-- [0,1,2] -> [(0,1),(1,2),(2,0)]
cyclicConsecutivePairs :: [a] -> [(a,a)]
cyclicConsecutivePairs [] = []
cyclicConsecutivePairs xs = cyclicPairs0 (head xs) xs
  where
    cyclicPairs0 _ [] = []
    cyclicPairs0 first [x] = [(x, first)]
    cyclicPairs0 first (x:y:xs) = (x,y) : cyclicPairs0 first (y:xs)


cyclicRemoveConsecutiveDuplicates :: Eq a => [a] -> [a]
cyclicRemoveConsecutiveDuplicates [] = []
cyclicRemoveConsecutiveDuplicates [x] = [x]
cyclicRemoveConsecutiveDuplicates xs =
  if l > 1 && head cleaned == last cleaned then
    take (l - 1) cleaned
  else
    cleaned
  where
    l = length cleaned
    cleaned = rec xs
    rec [] = []
    rec (x:xs) = x : (rec $ dropWhile (x==) xs)


-- group all b's by a's
associate :: Eq a => [(a, b)] -> [(a, [b])]
associate [] = []
associate ((x, i):r) = (x, i : map (\(_, j) -> j) xs) : associate nonXs
  where
    (xs, nonXs) = partition (\(y, _) -> x == y) r


-- flatten a list of lists, only keeping distinct elements.
-- note: duplicates originating from inside a single list will remain
concatDistinct :: Eq a => [[a]] -> [a]
concatDistinct = foldr union []


-- prepend to xs0 all of xs1 not in xs0.
-- duplicates may occur.
union :: Eq a => [a] -> [a] -> [a]
union xs0 xs1 = (filter (\f -> not $ elem f xs0) xs1) ++ xs0


intersection :: Eq a => [a] -> [a] -> [a]
intersection xs [] = []
intersection xs (y:ys) =
  if elem y xs && not (elem y rem)
    then y : rem
    else rem
  where rem = intersection xs ys


removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (h:t) =
  if elem h t
    then removeDups t
    else h : removeDups t


swapElems i j ls = [get k x | (k, x) <- zip [0..] ls]
  where get k x | k == i = ls !! j
                | k == j = ls !! i
                | otherwise = x


-- see unit tests for expected behavior
insertBetween :: Eq a => [a] -> a -> a -> [a] -> [a]
insertBetween toInsert after before [] = []
insertBetween toInsert after before [a] =
  if a == after then
    a : toInsert
  else if a == before then
    toInsert ++ [a]
  else
    [a]
insertBetween toInsert after before (a:b:l) =
  if a == after && b == before then
    (a : toInsert) ++ (b : l)
  else if a == before && b == after then
    (a : reverse toInsert) ++ (b : l)
  else if a == after then
    (reverse toInsert) ++ (a:b:l)
  else if a == before then
    toInsert ++ (a:b:l)
  else
    a : (insertBetween toInsert after before $ b:l)


