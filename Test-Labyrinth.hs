module Main where

import Test.Hspec

import Labyrinth
import FloretSphere (cube, tetrahedron)
import Geometry
import VoronoiCut

main :: IO ()
main = hspec $ do
  let laby0 = Leaf 0
  let laby1 = Node 1 [Leaf 2]
  let laby2 = Node 2 [Leaf 5, Leaf 4]
  let laby3 = Node 3 [Node 4 [Leaf 2]]

  let cubeLaby = Node 0 [Node 2 [Leaf 1], Leaf 4, Leaf 3, Leaf 5]

  let cubeVM = fromModel cube
  let cubeFaces = VoronoiCut.faces cubeVM

  describe "labyrinth size" $ do
    it "should work" $ do
      size laby0 `shouldBe` 1
      size laby1 `shouldBe` 2
      size laby2 `shouldBe` 3
      size laby3 `shouldBe` 3
      size cubeLaby `shouldBe` 6

  describe "labyrinth value" $ do
    it "should work" $ do
      value laby0 `shouldBe` 0
      value laby1 `shouldBe` 1
      value laby2 `shouldBe` 2
      value laby3 `shouldBe` 3
      value cubeLaby `shouldBe` 0

  describe "labyrinth to path vertice" $ do
    it "should work" $ do
      labyrinthToPathVertice cubeFaces laby0 `shouldBe` [Point3f 0 1 0]
      labyrinthToPathVertice cubeFaces laby1 `shouldBe` [Point3f 0 (-1) 0, Point3f 1 (-1) 0, Point3f 1 0 0]
      labyrinthToPathVertice cubeFaces laby2 `shouldBe` [Point3f 1 0 0,Point3f 1 0 (-1),Point3f 0 0 (-1),Point3f 1 0 1,Point3f 0 0 1]
      labyrinthToPathVertice cubeFaces laby3 `shouldBe` [Point3f (-1) 0 0,Point3f (-1) 0 1,Point3f 0 0 1,Point3f 1 0 1,Point3f 1 0 0]
      labyrinthToPathVertice cubeFaces cubeLaby `shouldBe` [Point3f 0 1 0,Point3f 1 1 0,Point3f 1 0 0,Point3f 1 (-1) 0,Point3f 0 (-1) 0,Point3f 0 1 1,Point3f 0 0 1,Point3f (-1) 1 0,Point3f (-1) 0 0,Point3f 0 1 (-1),Point3f 0 0 (-1)]

  describe "labyrinth to path indice" $ do
    it "should work" $ do
      labyrinthToPathIndice 0 laby0 `shouldBe` []
      labyrinthToPathIndice 0 laby1 `shouldBe` [0,1,1,2]
      labyrinthToPathIndice 0 laby2 `shouldBe` [0,3,3,4,0,1,1,2]
      labyrinthToPathIndice 0 laby3 `shouldBe` [0,1,1,2,2,3,3,4]
      labyrinthToPathIndice 0 cubeLaby `shouldBe` [0,9,9,10,0,7,7,8,0,5,5,6,0,1,1,2,2,3,3,4]

  describe "labyrinth to wall vertice" $ do
    it "should work" $ do
      labyrinthToWallVertice cubeFaces laby0 [] `shouldBe` [Point3f 1.0 1.0 1.0,Point3f 1.0 1.0 (-1.0),Point3f 1.0 1.0 (-1.0),Point3f (-1.0) 1.0 (-1.0),Point3f (-1.0) 1.0 (-1.0),Point3f (-1.0) 1.0 1.0,Point3f (-1.0) 1.0 1.0,Point3f 1.0 1.0 1.0]

  describe "labyrinth to wall indice" $ do
    it "should work" $ do
      labyrinthToWallIndice 0 (map neighbours $ faceList cubeVM) laby0 `shouldBe` ([0,1,2,3,4,5,6,7], 8)

