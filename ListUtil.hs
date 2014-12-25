module ListUtil where


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