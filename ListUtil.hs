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