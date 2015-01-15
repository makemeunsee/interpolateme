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
      cyclicRemoveConsecutiveDuplicates [0,1,2] `shouldBe` [0,1,2]
      cyclicRemoveConsecutiveDuplicates [0,0,1,2] `shouldBe` [0,1,2]
      cyclicRemoveConsecutiveDuplicates [0,1,2,0] `shouldBe` [1,2,0]
      cyclicRemoveConsecutiveDuplicates [0,0,0,0,0,1,1,1,1,2,2,2,2,3,4,5,1,1,0,0,0] `shouldBe` [1,2,3,4,5,1,0]