module Main where

import Test.Hspec
import Test.QuickCheck

import ListUtil

main :: IO ()
main = hspec $ do
  describe "intersection" $ do
    it "should be empty if one list is empty" $ property $
      \xs -> intersection xs [] == ([] :: [Int]) && intersection [] xs == ([] :: [Int])

    it "should be smaller than the sum of the sizes of the intersected" $ property $
      \xs ys -> (length $ intersection xs ys) <= (length ys + length (xs :: [Int]))

    it "should be commutative" $ property $
      \ xs ys -> (length $ intersection xs ys) == (length $ intersection ys (xs :: [Int]))

    it "should filter out duplicates" $ do
      intersection [0,1,2] [0,0,1,1,2,2] `shouldBe` [0,1,2]
      intersection [0,0,1,1,2,2] [0,1,2] `shouldBe` [0,1,2]

    it "should be empty when there are no common elements" $ do
      intersection [0,1,2] [3,4,5] `shouldBe` []
      intersection [0,1,2,3,4,5,6,7,8,9] [13] `shouldBe` []

  describe "cyclicRemoveConsecutiveDuplicates" $ do
    it "should remove consecutive duplicates, looping around the ends" $ do
      cyclicRemoveConsecutiveDuplicates [] `shouldBe` ([] :: [Int])
      cyclicRemoveConsecutiveDuplicates [0] `shouldBe` [0]
      cyclicRemoveConsecutiveDuplicates [1,1] `shouldBe` [1]
      cyclicRemoveConsecutiveDuplicates [0,1] `shouldBe` [0,1]
      cyclicRemoveConsecutiveDuplicates [0,1,2] `shouldBe` [0,1,2]
      cyclicRemoveConsecutiveDuplicates [0,0,1,2] `shouldBe` [0,1,2]
      cyclicRemoveConsecutiveDuplicates [0,0,1,2,2] `shouldBe` [0,1,2]
      cyclicRemoveConsecutiveDuplicates [0,1,2,0] `shouldBe` [0,1,2]
      cyclicRemoveConsecutiveDuplicates [0,0,0,0,0,1,1,1,1,2,2,2,2,3,4,5,1,1,0,0,0] `shouldBe` [0,1,2,3,4,5,1]

  describe "associate" $ do
    it "should associate b's to a's" $ do
      associate [] `shouldBe` ([] :: [(Int, [Int])])
      associate [(0,1)] `shouldBe` [(0, [1])]
      associate [(0,1), (0,2)] `shouldBe` [(0, [1,2])]
      associate [(0,1), (2,3)] `shouldBe` [(0, [1]), (2, [3])]

  describe "insert between" $ do
    it "should work as follow" $ do
      insertBetween [2,3] 1 4 [1,4] `shouldBe` [1,2,3,4]
      insertBetween [2,3] 1 4 [1,4,0] `shouldBe` [1,2,3,4,0]
      insertBetween [2,3] 1 4 [0,1,4] `shouldBe` [0,1,2,3,4]
      insertBetween [2,3] 1 4 [0,1,4,0] `shouldBe` [0,1,2,3,4,0]
      insertBetween [2,3] 1 4 [4,1] `shouldBe` [4,3,2,1]
      insertBetween [2,3] 1 4 [4,1,0] `shouldBe` [4,3,2,1,0]
      insertBetween [2,3] 1 4 [0,4,1] `shouldBe` [0,4,3,2,1]
      insertBetween [2,3] 1 4 [0,4,1,0] `shouldBe` [0,4,3,2,1,0]
      insertBetween [2,3] 1 4 [1,0,0,4] `shouldBe` [3,2,1,0,0,4]
      insertBetween [2,3] 1 4 [4,0,0,1] `shouldBe` [2,3,4,0,0,1]
      insertBetween [2,3] 1 4 [4] `shouldBe` [2,3,4]
      insertBetween [2,3] 1 4 [1] `shouldBe` [1,2,3]
      insertBetween [2,3] 1 4 [0] `shouldBe` [0]

