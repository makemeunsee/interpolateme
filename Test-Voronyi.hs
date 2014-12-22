module Main where

import Voronyi
import ListUtil
import Geometry ( Point3f (Point3f), add )
import qualified Geometry as G
import FloretSphere
import Data.Maybe ( fromJust, isJust )
import Data.List ( elemIndex, findIndices )
import Graphics.Rendering.OpenGL.GL ( GLfloat )

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

tolerance = 0.00001

testPlane :: [Point3f GLfloat] -> (Int, Plane GLfloat) -> IO ()
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
      segmentPlaneIntersection tolerance(Point3f 1 0 0) (Point3f 0 0 0) plane0 seed0 `shouldBe` OnPoint (Point3f 1 0 0)
      segmentPlaneIntersection tolerance (Point3f 1 0 0) (Point3f 5 8 9) plane0 seed0 `shouldBe` OnPoint (Point3f 1 0 0)
      segmentPlaneIntersection tolerance (Point3f 2 0 0) (Point3f 0 0 0) plane0 seed0 `shouldBe` OnSegment (Point3f 1 0 0)
      segmentPlaneIntersection tolerance (Point3f 0 0 0) (Point3f 2 2 0) plane0 seed0 `shouldBe` OnSegment (Point3f 1 1 0)
      segmentPlaneIntersection tolerance (Point3f 0 0 0) (Point3f 2 2.2 2.2) plane0 seed0 `shouldBe` OnSegment (Point3f 1 1.1 1.1)
      segmentPlaneIntersection tolerance (Point3f 0 0 0) (Point3f 0.9 0 0) plane0 seed0 `shouldBe` None

      let seed1 = Point3f ((/) (-sqrt 3) 3) ((/) (-sqrt 3) 3) ((/) (-sqrt 3) 3)
      let plane1 = tangentPlane seed1
      let i0 = fromJust $ getPoint $ segmentPlaneIntersection tolerance (Point3f 1 1 1) (Point3f (-1) (-1) (-1)) plane1 seed1
      G.dist i0 seed1 `shouldSatisfy` (0.00001 >)
      let i1 = fromJust $ getPoint $ segmentPlaneIntersection tolerance (Point3f 0 0 0) (Point3f (-10) 0 0) plane1 seed1
      let e1 = Point3f (-sqrt 3) 0 0
      G.dist i1 e1 `shouldSatisfy` (0.00001 >)
      let i2 = fromJust $ getPoint $ segmentPlaneIntersection tolerance (Point3f 0 0 0) (Point3f 0 (-10) 0) plane1 seed1
      let e2 = Point3f 0 (-sqrt 3) 0
      G.dist i2 e2 `shouldSatisfy` (0.00001 >)
      let i3 = fromJust $ getPoint $ segmentPlaneIntersection tolerance (Point3f 0 0 0) (Point3f 0 0 (-10)) plane1 seed1
      let e3 = Point3f 0 0 (-sqrt 3)
      G.dist i3 e3 `shouldSatisfy` (0.00001 >)
      segmentPlaneIntersection tolerance (Point3f 1 1 1) (Point3f (-0.5) (-0.5) (-0.5)) plane1 seed1 `shouldBe` None
      segmentPlaneIntersection tolerance (Point3f (-6) (-5) (-9.06)) (Point3f (-0.6) (-0.6) (-0.6)) plane1 seed1 `shouldBe` None


  describe "Validating buildFromPolygons function" $ do
    it "should rebuild self consistently" $ do
      let oldM@(VoronoiModel seeds oldVertice oldFaces _) = toVoronoiModel tetrahedron :: VoronoiModel Float
      let oldPolys = map (map (oldVertice !!)) oldFaces
      let (newVertice, newFaces) = reducePolygons oldPolys
      let newPolys = map (map (newVertice !!)) newFaces
      newPolys `shouldBe` oldPolys
      return ()


  describe "Validating buildPolygonFromSegments function" $ do
    it "should rebuild a square" $ do
      -- edges packed alone
      let edges0 = [[Point3f 1 0 1, Point3f (-1) 0 1], [Point3f (-1) 0 1, Point3f (-1) 0 (-1)], [Point3f (-1) 0 (-1), Point3f 1 0 (-1)], [Point3f 1 0 (-1), Point3f 1 0 1]]
      let rebuilt0 = buildPolygonFromSegments edges0
      rebuilt0 `shouldBe` [Point3f (-1) 0 (-1), Point3f 1 0 (-1), Point3f 1 0 1, Point3f (-1) 0 1]

      -- edges packed in pairs
      let edges1 = [[Point3f 1 0 1, Point3f (-1) 0 1, Point3f (-1) 0 (-1)], [Point3f (-1) 0 (-1), Point3f 1 0 (-1), Point3f 1 0 1]]
      let rebuilt1 = buildPolygonFromSegments edges1
      rebuilt1 `shouldBe` [Point3f 1 0 (-1), Point3f 1 0 1, Point3f (-1) 0 1, Point3f (-1) 0 (-1)]


  describe "bug: one face of tetrahedron not cut" $ do
    it "should have 3 out of 4 faces cut" $ do
      let seed@(Point3f sx sy sz) = G.normalized $ Point3f 5 1 1 :: Point3f GLfloat
      let plane@(Plane a b c d) = tangentPlane seed

      let oldM@(VoronoiModel _ oldVertice oldFaces _) = toVoronoiModel tetrahedron
      let actualFaces = map (map (oldVertice !!)) oldFaces

      let newM@(VoronoiModel _ newVertice newFaces _) = Voronyi.truncate tolerance seed oldM
      let newActualFaces = map (map (newVertice !!)) newFaces

      length (newFaces !! 1) `shouldBe` 3
      length (newFaces !! 2) `shouldBe` 4
      length (newFaces !! 3) `shouldBe` 3
      length (newFaces !! 4) `shouldBe` 3

      length (newFaces !! 0) `shouldBe` 3


  describe "bug: coplanar cut" $ do
    it "coplanar cut should not crash" $ do
      let seed@(Point3f sx sy sz) = G.normalized $ Point3f (-1) 1 1 :: Point3f GLfloat
      let plane@(Plane a b c d) = tangentPlane seed

      let oldM@(VoronoiModel _ oldVertice oldFaces _) = toVoronoiModel tetrahedron
      let actualFaces = map (map (oldVertice !!)) oldFaces

      let newM@(VoronoiModel _ newVertice newFaces _) = Voronyi.truncate tolerance seed oldM
      let newActualFaces = map (map (newVertice !!)) newFaces

      length (newFaces !! 0) `shouldBe` 3
      length (newFaces !! 1) `shouldBe` 3
      length (newFaces !! 2) `shouldBe` 3
      length (newFaces !! 3) `shouldBe` 3


  describe "Bug: truncate twice" $ do
    it "should not crash when truncating twice in a row" $ do
      let seed@(Point3f sx sy sz) = G.normalized $ Point3f 1 1 1 :: Point3f GLfloat
      let plane@(Plane a b c d) = tangentPlane seed

      let m0 = toVoronoiModel tetrahedron
      let m1 = Voronyi.truncate tolerance seed m0
      let m2 = Voronyi.truncate tolerance seed m1

      m2 `shouldBe` m1


  describe "Validating cutPolygon function" $ do
    let seed0 = Point3f 1 0 0
    let plane0 = tangentPlane seed0

    let seed0' = Point3f (-1) 0 0
    let plane0' = tangentPlane seed0'

    it "should not cut unconcerned polygons" $ do
      let verticeIntact0 = [Point3f 0 0 0, Point3f 0 0 1, Point3f (-1) 0 1, Point3f (-1) 0 0]
      let faceIntact0 = map (verticeIntact0 !!) [0,1,2,3]
      let faceIntact0' = map (verticeIntact0 !!) [0,3,2,1]
      let noCut0 = cutPolygon tolerance faceIntact0 plane0 seed0
      let noCut0' = cutPolygon tolerance faceIntact0' plane0 seed0
      fst noCut0 `shouldBe` faceIntact0
      fst noCut0' `shouldBe` faceIntact0'

      let verticeIntact1 = [Point3f 3 0 0, Point3f 3 0 1, Point3f 2 0 1, Point3f 2 0 0]
      let faceIntact1 = map (verticeIntact1 !!) [0,1,2,3]
      let faceIntact1' = map (verticeIntact1 !!) [0,3,2,1]
      let noCut1 = cutPolygon tolerance faceIntact1 plane0 seed0
      let noCut1' = cutPolygon tolerance faceIntact1' plane0 seed0
      fst noCut1 `shouldBe` faceIntact1
      fst noCut1' `shouldBe` faceIntact1'

    it "should cut traversed polygons" $ do
      -- quads
      let vertice0 = [Point3f 0 0 0, Point3f 0 0 2, Point3f 2 0 2, Point3f 2 0 0]
      let face0 = map (vertice0 !!) [0,1,2,3]
      let face0' = map (vertice0 !!) [0,3,2,1]
      let cut0 = cutPolygon tolerance face0 plane0 seed0
      let cut0' = cutPolygon tolerance face0' plane0 seed0
      fst cut0 `shouldBe` [Point3f 0 0 0, Point3f 0 0 2, Point3f 1 0 2, Point3f 1 0 0]
      fst cut0' `shouldBe` [Point3f 0 0 0, Point3f 1 0 0, Point3f 1 0 2, Point3f 0 0 2]

      -- triangles, remove 1 vertex
      let vertice1 = [Point3f 0 0 (-2), Point3f 2 0 0, Point3f 0 0 2]
      let face1 = map (vertice1 !!) [0,1,2]
      let face1' = map (vertice1 !!) [2,1,0]
      let cut1 = cutPolygon tolerance face1 plane0 seed0
      let cut1' = cutPolygon tolerance face1' plane0 seed0
      fst cut1 `shouldBe` [Point3f 0 0 (-2), Point3f 1 0 (-1), Point3f 1 0 1, Point3f 0 0 2]
      fst cut1' `shouldBe` [Point3f 0 0 2, Point3f 1 0 1, Point3f 1 0 (-1), Point3f 0 0 (-2)]

      -- triangles, remove 2 vertice
      let vertice2 = [Point3f 0 0 0, Point3f 2 0 2, Point3f 2 0 (-2)]
      let face2 = map (vertice2 !!) [0,1,2]
      let face2' = map (vertice2 !!) [2,1,0]
      let cut2 = cutPolygon tolerance face2 plane0 seed0
      let cut2' = cutPolygon tolerance face2' plane0 seed0
      fst cut2 `shouldBe` [Point3f 0 0 0, Point3f 1 0 1, Point3f 1 0 (-1)]
      fst cut2' `shouldBe` [Point3f 1 0 (-1), Point3f 1 0 1, Point3f 0 0 0]

      -- quads, symmetric test
      let vertice3 = [Point3f 0 0 0, Point3f 0 0 2, Point3f (-2) 0 2, Point3f (-2) 0 0]
      let face3 = map (vertice3 !!) [0,1,2,3]
      let face3' = map (vertice3 !!) [0,3,2,1]
      let cut3 = cutPolygon tolerance face3 plane0' seed0'
      let cut3' = cutPolygon tolerance face3' plane0' seed0'
      fst cut3 `shouldBe` [Point3f 0 0 0, Point3f 0 0 2, Point3f (-1) 0 2, Point3f (-1) 0 0]
      fst cut3' `shouldBe` [Point3f 0 0 0, Point3f (-1) 0 0, Point3f (-1) 0 2, Point3f 0 0 2]

        -- triangles, remove 1 vertex, symmetric test
      let vertice4 = [Point3f 0 0 (-2), Point3f (-2) 0 0, Point3f 0 0 2]
      let face4 = map (vertice4 !!) [0,1,2]
      let face4' = map (vertice4 !!) [2,1,0]
      let cut4 = cutPolygon tolerance face4 plane0' seed0'
      let cut4' = cutPolygon tolerance face4' plane0' seed0'
      fst cut4 `shouldBe` [Point3f 0 0 (-2), Point3f (-1) 0 (-1), Point3f (-1) 0 1, Point3f 0 0 2]
      fst cut4' `shouldBe` [Point3f 0 0 2, Point3f (-1) 0 1, Point3f (-1) 0 (-1), Point3f 0 0 (-2)]

      -- triangles, remove 2 vertice, symmetric test
      let vertice5 = [Point3f 0 0 0, Point3f (-2) 0 2, Point3f (-2) 0 (-2)]
      let face5 = map (vertice5 !!) [0,1,2]
      let face5' = map (vertice5 !!) [2,1,0]
      let cut5 = cutPolygon tolerance face5 plane0' seed0'
      let cut5' = cutPolygon tolerance face5' plane0' seed0'
      fst cut5 `shouldBe` [Point3f 0 0 0, Point3f (-1) 0 1, Point3f (-1) 0 (-1)]
      fst cut5' `shouldBe` [Point3f (-1) 0 (-1), Point3f (-1) 0 1, Point3f 0 0 0]

  describe "Validating cutPolygon function on edges" $ do
    let face = [Point3f 1.5 0 0, Point3f 1 0 1, Point3f 0 0 1.5, Point3f (-1) 0 1, Point3f (-1.5) 0 0, Point3f (-1) 0 (-1), Point3f 0 0 (-1.5), Point3f 1 0 (-1)]
    let seed0 = Point3f 1 0 0
    let plane0 = tangentPlane seed0
    let seed1 = Point3f 0 0 1
    let plane1 = tangentPlane seed1
    let seed2 = Point3f (-1) 0 0
    let plane2 = tangentPlane seed2
    let seed3 = Point3f 0 0 (-1)
    let plane3 = tangentPlane seed3

    it "should handle 2 edges cases" $ do

      let cut0 = cutPolygon tolerance face plane0 seed0
      fst cut0 `shouldBe` map (face !!) [1,2,3,4,5,6,7]

      let cut1 = cutPolygon tolerance face plane1 seed1
      fst cut1 `shouldBe` map (face !!) [0,1,3,4,5,6,7]

      let cut2 = cutPolygon tolerance face plane2 seed2
      fst cut2 `shouldBe` map (face !!) [0,1,2,3,5,6,7]

      let cut3 = cutPolygon tolerance face plane3 seed3
      fst cut3 `shouldBe` map (face !!) [0,1,2,3,4,5,7]


    let face' = tail face ++ [head face]
    it "should handle 2 edges special cases" $ do

      let cut0 = cutPolygon tolerance face' plane0 seed0
      fst cut0 `shouldBe` map (face' !!) [0,1,2,3,4,5,6]

      let cut1 = cutPolygon tolerance face' plane1 seed1
      fst cut1 `shouldBe` map (face' !!) [0,2,3,4,5,6,7]

    let horn0 = [Point3f 1 0 0, Point3f 1 0 (-1), Point3f (-2) 0 (-1), Point3f (-1) 0 0]
    let horn1 = [Point3f 1 0 0, Point3f 1 0 (-1), Point3f (-1) 0 (-1), Point3f (-2) 0 0]
    it "should handle 1 edges & 1 segment cases" $ do

      let cut0 = cutPolygon tolerance horn0 plane2 seed2
      fst cut0 `shouldBe` [horn0 !! 0, horn0 !! 1, Point3f (-1) 0 (-1), horn0 !! 3]

      let cut1 = cutPolygon tolerance horn1 plane2 seed2
      fst cut1 `shouldBe` [horn1 !! 0, horn1 !! 1, horn1 !! 2, Point3f (-1) 0 0]

    let horn2 = [Point3f 1 0 0, Point3f 2 0 (-1), Point3f 0 0 (-1), Point3f 0 0 0]
    let horn3 = [Point3f 1 0 0, Point3f 2 0 1, Point3f 0 0 1, Point3f 0 0 0]
    it "should handle 1 edges & 1 segment special cases" $ do

      let cut2 = cutPolygon tolerance horn2 plane0 seed0
      fst cut2 `shouldBe` [horn2 !! 0, Point3f 1 0 (-1), horn2 !! 2, horn2 !! 3]

      let cut3 = cutPolygon tolerance horn3 plane0 seed0
      fst cut3 `shouldBe` [horn3 !! 0, Point3f 1 0 1, horn3 !! 2, horn3 !! 3]


  describe "Validating cutEdge function" $ do
    it "should cut the unit segment" $ do
      let seed = Point3f 1 0 0
      let plane = tangentPlane seed

      -- plane touches 1 0 0
      cutEdge tolerance plane seed 4 ((0, Point3f 0 0 0, [0,1,2]), (1, Point3f 1 0 0, [0,2,3])) `shouldBe` ([(0, Point3f 0 0 0, [0,1,2]) , (1, Point3f 1 0 0, [0,2,3,4])], [])
      cutEdge tolerance plane seed 4 ((0, Point3f 1 0 0, [0,1,2]), (1, Point3f 0 0 0, [0,2,3])) `shouldBe` ([(0, Point3f 1 0 0, [0,1,2,4]) , (1, Point3f 0 0 0, [0,2,3])], [])

      cutEdge tolerance plane seed 4 ((0, Point3f 1 0 0, [0,1,2]), (1, Point3f 2 0 0, [0,2,3])) `shouldBe` ([(0, Point3f 1 0 0, [0,1,2,4])], [1])
      cutEdge tolerance plane seed 4 ((0, Point3f 2 0 0, [0,1,2]), (1, Point3f 1 0 0, [0,2,3])) `shouldBe` ([(1, Point3f 1 0 0, [0,2,3,4])], [0])

      cutEdge tolerance plane seed 4 ((0, Point3f 0.5 0 0, [0,1,2]), (1, Point3f 1.5 0 0, [0,2,3])) `shouldBe` ([(0, Point3f 0.5 0 0, [0,1,2]), ((-1), Point3f 1 0 0, [0,2,4])], [1])
      cutEdge tolerance plane seed 4 ((0, Point3f 1.5 0 0, [0,1,2]), (1, Point3f 0.5 0 0, [0,2,3])) `shouldBe` ([((-1), Point3f 1 0 0, [0,2,4]), (1, Point3f 0.5 0 0, [0,2,3])], [0])

      cutEdge tolerance plane seed 4 ((0, Point3f 1.5 0 0, [0,1,2]), (1, Point3f 2.5 0 0, [0,2,3])) `shouldBe` ([], [0,1])
      cutEdge tolerance plane seed 4 ((0, Point3f 2.5 0 0, [0,1,2]), (1, Point3f 1.5 0 0, [0,2,3])) `shouldBe` ([], [0,1])

      cutEdge tolerance plane seed 4 ((0, Point3f (-0.5) 0 0, [0,1,2]), (1, Point3f 0.5 0 0, [0,2,3])) `shouldBe` ([(0, Point3f (-0.5) 0 0, [0,1,2]), (1, Point3f 0.5 0 0, [0,2,3])], [])
      cutEdge tolerance plane seed 4 ((0, Point3f 0.5 0 0, [0,1,2]), (1, Point3f (-0.5) 0 0, [0,2,3])) `shouldBe` ([(0, Point3f 0.5 0 0, [0,1,2]), (1, Point3f (-0.5) 0 0, [0,2,3])], [])

  describe "Validating cutFace function" $ do
    it "should cut a triangle" $ do
      let p0 = Point3f 1 3 0
      let p0' = Point3f 0 3 0
      let p1 = Point3f (-1) (-1) 0
      let p2 = Point3f 2 (-1) 0
      let p2' = Point3f 1 0 0
      let allVertice = [p0,p1,p2]
      let allVertice' = [p0',p1,p2']
      let facesOfVertice = [[0,1,2],[0,2,3],[0,1,3]] -- fake references to tetrahedron faces

      let seed0 = Point3f 1 0 0
      let plane0 = tangentPlane seed0
      let seed1 = Point3f 0 1 0
      let plane1 = tangentPlane seed1

      cutFace plane0 seed0 allVertice facesOfVertice 4 3 [0,1,2] `shouldBe` ([0,1,3], [ (0, Point3f 1 3 0, [0,1,2,4]), (3, Point3f 1 (-1) 0, [0,3,4])], [2])

      cutFace plane1 seed1 allVertice facesOfVertice 4 3 [0,1,2] `shouldBe` ([4,1,2,3], [(4, Point3f 0 1 0, [0,2,4]), (3, Point3f 1.5 1 0, [0,1,4])], [0])

      cutFace plane0 seed0 allVertice' facesOfVertice 4 3 [0,1,2] `shouldBe` ([0,1,2], [(2, Point3f 1 0 0, [0,1,3,4])], [])

    it "should cut a polygon" $ do
      let seed = Point3f 0 1 0
      let plane = tangentPlane seed

      let p0 = Point3f 0 0 0
      let p1 = Point3f (-1) (-1) 0
      let p2 = Point3f (-1) (-2) 0
      let p3 = Point3f 0 (-3) 0
      let p4 = Point3f 2 (-2) 0
      let all = [p0,p1,p2,p3,p4]
      let face = [0,1,2,3,4]
      let facesOfVertice = [[0,1,5], [0,1,2], [0,2,3], [0,3,4], [0,4,5]]

      -- cut plane fully above polygon
      cutFace plane seed all facesOfVertice 6 5 face `shouldBe` (face, [], [])

      -- cut plane touches polygon top vertex
      let all1 = map (add (Point3f 0 1 0)) all
      cutFace plane seed all1 facesOfVertice 6 5 face `shouldBe` (face, [(0, Point3f 0 1 0, [0,1,5,6])], [])

      -- cut plane truncates top vertex of polygon, through 2 segments
      let all2 = map (add (Point3f 0 1.5 0)) all
      cutFace plane seed all2 facesOfVertice 6 5 face `shouldBe` ( [6,1,2,3,4,5]
                                                                 , [ (6, Point3f (-0.5) 1 0, [0,1,6])
                                                                   , (5, Point3f 0.5 1 0, [0,5,6])
                                                                   ]
                                                                 , [0])

      -- cut plane truncates top vertex of polygon, through 1 vertex and 1 segment
      let all3 = map (add (Point3f 0 2 0)) all
      cutFace plane seed all3 facesOfVertice 6 5 face `shouldBe` ( [1,2,3,4,5]
                                                                 , [ (1, Point3f (-1) 1 0, [0,1,2,6])
                                                                   , (5, Point3f 1 1 0, [0,5,6])
                                                                   ]
                                                                 , [0])

      -- cut plane truncates 2 top vertice of polygon, through 2 vertice
      let all4 = map (add (Point3f 0 3 0)) all
      cutFace plane seed all4 facesOfVertice 6 5 face `shouldBe` ( [2,3,4]
                                                                 , [ (2, Point3f (-1) 1 0, [0,2,3,6])
                                                                   , (4, Point3f 2 1 0, [0,4,5,6])
                                                                   ]
                                                                 , [0,1])

      -- cut plane truncates all but bottom vertex of polygon, through bottom vertex
      let all5 = map (add (Point3f 0 4 0)) all
      cutFace plane seed all5 facesOfVertice 6 5 face `shouldBe` ( [3]
                                                                 , [ (3, Point3f 0 1 0, [0,3,4,6])
                                                                   ]
                                                                 , [4,2,0,1])

      -- cut plane fully below polygon
      let all6 = map (add (Point3f 0 5 0)) all
      cutFace plane seed all6 facesOfVertice 6 5 face `shouldBe` ( []
                                                                 , []
                                                                 , [4,3,2,0,1])
