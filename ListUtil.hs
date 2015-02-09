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
    cleaned = rec0 xs
    rec0 [] = []
    rec0 (x:xs) = x : rec0 (dropWhile (x==) xs)


-- group all b's by a's
associate :: Eq a => [(a, b)] -> [(a, [b])]
associate [] = []
associate ((x, i):r) = (x, i : map snd xs) : associate nonXs
  where
    (xs, nonXs) = partition (\(y, _) -> x == y) r


-- flatten a list of lists, only keeping distinct elements.
-- note: duplicates originating from inside a single list will remain
concatDistinct :: Eq a => [[a]] -> [a]
concatDistinct = foldr union []


-- prepend to xs0 all of xs1 not in xs0.
-- duplicates may occur.
union :: Eq a => [a] -> [a] -> [a]
union xs0 xs1 = filter (`notElem` xs0) xs1 ++ xs0


intersection :: Eq a => [a] -> [a] -> [a]
intersection _ [] = []
intersection xs (y:ys) =
  if elem y xs && notElem y rem
    then y : rem
    else rem
  where rem = intersection xs ys


removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (h:t) =
  if h `elem` t
    then removeDups t
    else h : removeDups t


swapElems :: Int -> Int -> [a] -> [a]
swapElems i j ls = [get k x | (k, x) <- zip [0..] ls]
  where get k x | k == i = ls !! j
                | k == j = ls !! i
                | otherwise = x


-- see unit tests for expected behavior
insertBetween :: Eq a => [a] -> a -> a -> [a] -> [a]
insertBetween _ _ _ [] = []
insertBetween toInsert after before [a]
  | a == after = a : toInsert
  | a == before = toInsert ++ [a]
  | otherwise = [a]
insertBetween toInsert after before (a:b:l)
  | a == after && b == before = (a : toInsert) ++ (b : l)
  | a == before && b == after = (a : reverse toInsert) ++ (b : l)
  | a == after = reverse toInsert ++ (a:b:l)
  | a == before = toInsert ++ (a:b:l)
  | otherwise = a : insertBetween toInsert after before (b:l)
