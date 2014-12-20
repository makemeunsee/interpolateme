module Main where

import Geometry
import FloretSphere
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Validating neighbour function" $ do
    it "should compute the neighbours of the tetrahedron faces" $ do
      neighbours tetrahedron `shouldBe` [[1,2,3],[0,2,3],[0,1,3],[0,1,2]]

    it "should compute the neighbours of the cube faces" $ do
      neighbours cube `shouldBe` [[2,3],[2,3],[0,1],[0,1]]

    it "should compute the neighbours of the dodecahedron faces" $ do
      neighbours dodecahedron `shouldBe` [ [1,2,7,9,11]
                                         , [0,6,7,9,10]
                                         , [0,3,8,9,11]
                                         , [2,4,8,9,10]
                                         , [3,5,6,8,10]
                                         , [4,6,7,8,11]
                                         , [1,4,5,7,10]
                                         , [0,1,5,6,11]
                                         , [2,3,4,5,11]
                                         , [0,1,2,3,10]
                                         , [1,3,4,6,9]
                                         , [0,2,5,7,8]]