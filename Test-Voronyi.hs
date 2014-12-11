module Main where

import Voronyi
import Geometry ( Point3f (Point3f), dist )
import FloretSphere
import Data.Maybe ( fromJust )

import Test.Hspec

p0 = Point3f 1 0 0
p1 = Point3f (-1) 0 0
p2 = Point3f 0 1 0
p3 = Point3f 0 (-1) 0
p4 = Point3f 0 0 1
p5 = Point3f 0 0 (-1)
points = [p0, p1, p2, p3, p4, p5]

planes = map tangentPlane points

tetraVoro = toVoronoiModel tetrahedron

testPlane :: [Point3f Float] -> (Int, Plane Float) -> IO ()
testPlane pts (i, pl) = do
  let pi@(Point3f x y z) = pts !! i
  pl `shouldBe` (Plane x y z (-1))
  onPlane pl pi `shouldBe` True
  mapM (\p -> onPlane pl p `shouldBe` False) $ filter(pi /=) pts
  return ()


main :: IO ()
main = hspec $ do
  describe "Validating planes" $ do
    it "should create consistent planes" $ do
      mapM (testPlane points) $ zip [0..] planes
      return ()

  describe "Validating closestSeed function" $ do
    it "should find the correct seed" $ do
      -- on face 0
      closestSeed tetraVoro (Point3f 1 1 (-1)) `shouldBe` (seeds tetraVoro !! 0)
      -- on face 1
      closestSeed tetraVoro (Point3f (-1) 1 1) `shouldBe` (seeds tetraVoro !! 1)
      -- on face 2
      closestSeed tetraVoro (Point3f 1 (-1) 1) `shouldBe` (seeds tetraVoro !! 2)
      -- on face 3
      closestSeed tetraVoro (Point3f (-1) (-1) (-1)) `shouldBe` (seeds tetraVoro !! 3)

  describe "Validating wallsOfSeed function" $ do
    it "should return the polygon for some index" $ do
      -- face 0 has indice 0,2,1
      wallsOfSeed tetraVoro 0 `shouldBe` map (vertice tetraVoro !!) [0,2,1]
      -- face 1 has indice 0,1,3
      wallsOfSeed tetraVoro 1 `shouldBe` map (vertice tetraVoro !!) [0,1,3]
      -- face 2 has indice 0,3,2
      wallsOfSeed tetraVoro 2 `shouldBe` map (vertice tetraVoro !!) [0,3,2]
      -- face 3 has indice 1,2,3
      wallsOfSeed tetraVoro 3 `shouldBe` map (vertice tetraVoro !!) [1,2,3]

  describe "Validating medianPlane function" $ do
    it "should return a consistent plane" $ do
      let plane0 = medianPlane (Point3f 1 0 0) (Point3f (-1) 0 0)
      onPlane plane0 (Point3f 0 0 0) `shouldBe` True
      onPlane plane0 (Point3f 0 5 0) `shouldBe` True
      onPlane plane0 (Point3f 0 0 (-4)) `shouldBe` True
      onPlane plane0 (Point3f 0 7 2) `shouldBe` True
      onPlane plane0 (Point3f 1 0 0) `shouldBe` False
      onPlane plane0 (Point3f 2 5 0) `shouldBe` False
      onPlane plane0 (Point3f 0.1 0 (-4)) `shouldBe` False
      onPlane plane0 (Point3f (-1.1) 7 2) `shouldBe` False

      let plane1 = medianPlane (Point3f 1 1 1) (Point3f (-1) (-1) (-1))
      onPlane plane1 (Point3f 0 0 0) `shouldBe` True
      onPlane plane1 (Point3f 1 0 (-1)) `shouldBe` True
      onPlane plane1 (Point3f (-1.1) 0 1.1) `shouldBe` True
      onPlane plane1 (Point3f 1 0 0) `shouldBe` False

  describe "Validating tangentPlane function" $ do
    it "should return a consistent plane" $ do
      let plane0 = tangentPlane (Point3f 2 0 0)
      onPlane plane0 (Point3f 1 0 0) `shouldBe` True
      onPlane plane0 (Point3f 1 5 2) `shouldBe` True
      onPlane plane0 (Point3f 1 0 (-4)) `shouldBe` True
      onPlane plane0 (Point3f 1 7.1 0) `shouldBe` True
      onPlane plane0 (Point3f 0.1 0 0) `shouldBe` False
      onPlane plane0 (Point3f 0 5 0) `shouldBe` False
      onPlane plane0 (Point3f (-1) 0 (-4)) `shouldBe` False
      onPlane plane0 (Point3f (-1.1) 7 2) `shouldBe` False

      let plane1 = tangentPlane (Point3f 1 1 1)
      onPlane plane1 (Point3f ((/) (sqrt 3) 3) ((/) (sqrt 3) 3) ((/) (sqrt 3) 3)) `shouldBe` True
      onPlane plane1 (Point3f 0 0 0) `shouldBe` False
      onPlane plane1 (Point3f 0 0 (sqrt 3)) `shouldBe` True
      onPlane plane1 (Point3f 0 (sqrt 3) 0) `shouldBe` True
      onPlane plane1 (Point3f (sqrt 3) 0 0) `shouldBe` True

  describe "Validating segmentPlaneIntersection function" $ do
    it "should return correct intersections" $ do
      let seed0 = Point3f 1 0 0
      let plane0 = tangentPlane seed0
      segmentPlaneIntersection (Point3f 1 0 0) (Point3f 0 0 0) plane0 seed0 `shouldBe` Just (Point3f 1 0 0)
      segmentPlaneIntersection (Point3f 1 0 0) (Point3f 5 8 9) plane0 seed0 `shouldBe` Just (Point3f 1 0 0)
      segmentPlaneIntersection (Point3f 2 0 0) (Point3f 0 0 0) plane0 seed0 `shouldBe` Just (Point3f 1 0 0)
      segmentPlaneIntersection (Point3f 0 0 0) (Point3f 2 2 0) plane0 seed0 `shouldBe` Just (Point3f 1 1 0)
      segmentPlaneIntersection (Point3f 0 0 0) (Point3f 2 2.2 2.2) plane0 seed0 `shouldBe` Just (Point3f 1 1.1 1.1)
      segmentPlaneIntersection (Point3f 0 0 0) (Point3f 0.9 0 0) plane0 seed0 `shouldBe` Nothing

      let seed1 = Point3f ((/) (-sqrt 3) 3) ((/) (-sqrt 3) 3) ((/) (-sqrt 3) 3)
      let plane1 = tangentPlane seed1
      let i0 = fromJust $ segmentPlaneIntersection (Point3f 1 1 1) (Point3f (-1) (-1) (-1)) plane1 seed1
      dist i0 seed1 `shouldSatisfy` (0.00001 >)
      let i1 = fromJust $ segmentPlaneIntersection (Point3f 0 0 0) (Point3f (-10) 0 0) plane1 seed1
      let e1 = Point3f (-sqrt 3) 0 0
      dist i1 e1 `shouldSatisfy` (0.00001 >)
      let i2 = fromJust $ segmentPlaneIntersection (Point3f 0 0 0) (Point3f 0 (-10) 0) plane1 seed1
      let e2 = Point3f 0 (-sqrt 3) 0
      dist i2 e2 `shouldSatisfy` (0.00001 >)
      let i3 = fromJust $ segmentPlaneIntersection (Point3f 0 0 0) (Point3f 0 0 (-10)) plane1 seed1
      let e3 = Point3f 0 0 (-sqrt 3)
      dist i3 e3 `shouldSatisfy` (0.00001 >)
      segmentPlaneIntersection (Point3f 1 1 1) (Point3f (-0.5) (-0.5) (-0.5)) plane1 seed1 `shouldBe` Nothing
      segmentPlaneIntersection (Point3f (-6) (-5) (-9.06)) (Point3f (-0.6) (-0.6) (-0.6)) plane1 seed1 `shouldBe` Nothing