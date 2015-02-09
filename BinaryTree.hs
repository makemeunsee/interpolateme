module BinaryTree

where

data BTree a = BTNode a (Maybe (BTree a)) (Maybe (BTree a))


insert :: (Eq a, Ord a) => a -> BTree a -> BTree a
insert x t@(BTNode y l r)
  | x == y = t
  | x < y = case l of
              Just lt -> BTNode y (Just $ insert x lt) r
              Nothing -> BTNode y (Just $ BTNode x Nothing Nothing) r
  | otherwise = case r of
                  Just rt -> BTNode y l (Just $ insert x rt)
                  Nothing -> BTNode y l (Just $ BTNode x Nothing Nothing)


elem :: (Eq a, Ord a) => a -> BTree a -> Bool
elem x (BTNode y l r) = (x == y) ||
  if x < y then
    case l of
      Just lt -> BinaryTree.elem x lt
      Nothing -> False
  else
    case r of
      Just rt -> BinaryTree.elem x rt
      Nothing -> False
