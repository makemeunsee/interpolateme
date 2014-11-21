module ListUtil where


chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)


contains :: Eq a => a -> [a] -> Bool
contains x xs = any (\e -> x == e) xs


notContains :: Eq a => a -> [a] -> Bool
notContains x xs = all (\e -> x /= e) xs


cyclicConsecutivePairs :: [a] -> [(a,a)]
cyclicConsecutivePairs [] = []
cyclicConsecutivePairs xs = cyclicPairs0 (head xs) xs
  where
    cyclicPairs0 _ [] = []
    cyclicPairs0 first [x] = [(x, first)]
    cyclicPairs0 first (x:y:xs) = (x,y) : cyclicPairs0 first (y:xs)