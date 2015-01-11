module Main where

import Geometry
import FloretSphere
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Validating edge neighbour function" $ do
    it "should compute the edge neighbours of the tetrahedron faces" $ do
      edgeNeighbours tetrahedron `shouldBe` [[1,2,3],[0,2,3],[0,1,3],[0,1,2]]

    it "should compute the edge neighbours of the cube faces" $ do
      edgeNeighbours cube `shouldBe` [[2,3,4,5],[2,3,4,5],[0,1,4,5],[0,1,4,5],[0,1,2,3],[0,1,2,3]]

    it "should compute the edge neighbours of the dodecahedron faces" $ do
      edgeNeighbours dodecahedron `shouldBe` [ [1,2,7,9,11]
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

  describe "Validating vertex neighbour function" $ do
    it "should compute the vertex neighbours of the tetrahedron faces" $ do
      vertexNeighbours tetrahedron `shouldBe` [[1,2,3],[0,2,3],[0,1,3],[0,1,2]]

    it "should compute the vertex neighbours of the cube faces" $ do
      vertexNeighbours cube `shouldBe` [[2,3,4,5],[2,3,4,5],[0,1,4,5],[0,1,4,5],[0,1,2,3],[0,1,2,3]]

    it "should compute the vertex neighbours of the dodecahedron faces" $ do
      vertexNeighbours dodecahedron `shouldBe` [ [1,2,7,9,11]
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

    it "should find the right count of vertex neighbours for the icosahedron faces" $ do
      map length (vertexNeighbours icosahedron) `shouldBe` (take 20 $ cycle [9])


  describe "Validating facesForEachVertex function" $ do
    it "should compute the face ids for each vertex of the tetrahedron" $ do
      facesForEachVertex tetrahedron `shouldBe` [[0,1,2],[0,1,3],[0,2,3],[1,2,3]]

    it "should compute the face ids for each vertex of the dodecahedron" $ do
      facesForEachVertex dodecahedron `shouldBe` [ [0,1,9]
                                                 , [0,2,11]
                                                 , [1,6,7]
                                                 , [5,7,11]
                                                 , [3,9,10]
                                                 , [2,3,8]
                                                 , [4,6,10]
                                                 , [4,5,8]
                                                 , [1,9,10]
                                                 , [2,8,11]
                                                 , [1,6,10]
                                                 , [5,8,11]
                                                 , [0,2,9]
                                                 , [5,6,7]
                                                 , [2,3,9]
                                                 , [4,5,6]
                                                 , [0,1,7]
                                                 , [0,7,11]
                                                 , [3,4,10]
                                                 , [3,4,8]
                                                 ]