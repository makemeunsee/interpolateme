module Main where

import Test.Hspec

import VoronoiCut
import PlaneCut ( Plane (..) )
import Geometry
import FloretSphere

main :: IO ()
main = hspec $ do
  describe "validating closestSeed function" $ do
    it "find cube centers" $ do
      let m = cube
      let vm = fromModel m
      closestSeed vm ( normalized $ Point3f (1.1) (-0.1) 0 ) `shouldBe` (2, Point3f 1 0 0)
      closestSeed vm ( normalized $ Point3f (0.1) (0.9) 0 ) `shouldBe` (0, Point3f 0 1 0)
      closestSeed vm ( normalized $ Point3f (-0.1) 0 1 ) `shouldBe` (4, Point3f 0 0 1)
      closestSeed vm ( normalized $ Point3f (-0.9) 0 0 ) `shouldBe` (3, Point3f (-1) 0 0)
      closestSeed vm ( normalized $ Point3f (-0.1) (-1.1) 0 ) `shouldBe` (1, Point3f 0 (-1) 0)
      closestSeed vm ( normalized $ Point3f (0.1) 0 (-1.1) ) `shouldBe` (5, Point3f 0 0 (-1))

  describe "cutFace" $ do
    let center = Point3f 0 0 0
    let f = Face center [Point3f 1 0 0, Point3f 0 1 0, Point3f (-1) 0 0, Point3f 0 (-1) 0] []
    let f' = Face center [Point3f 1 1 0, Point3f (-1) 1 0, Point3f (-1) (-1) 0, Point3f 1 (-1) 0] []

    it "should not cut a face entirely below the cutting plane" $ do
      let p = Plane 1 0 0 $ Point3f 2 0 0
      let p' = Plane 1 0 0 $ Point3f 1 0 0
      cutFace 0 f p `shouldBe` (f, [])
      cutFace 0 f p' `shouldBe` (Face center (VoronoiCut.vertice f) [0], [Point3f 1 0 0])
      cutFace 0 f' p `shouldBe` (f', [])
      cutFace 0 f' p' `shouldBe` (Face center (VoronoiCut.vertice f') [0], [Point3f 1 (-1) 0, Point3f 1 1 0])

    it "should empty a face entirely above the cutting plane" $ do
      let p = Plane 1 0 0 $ Point3f (-2) 0 0
      let p' = Plane 1 0 0 $ Point3f (-1) 0 0
      cutFace 0 f p `shouldBe` (Face center [] [], [])
      cutFace 0 f p' `shouldBe` (Face center [Point3f (-1) 0 0] [0], [Point3f (-1) 0 0])
      cutFace 0 f' p `shouldBe` (Face center [] [], [])
      cutFace 0 f' p' `shouldBe` (Face center [Point3f (-1) 1 0, Point3f (-1) (-1) 0] [0], [Point3f (-1) 1 0, Point3f (-1) (-1) 0])

    it "should cut a face intersecting the plane" $ do
      let p = Plane 1 0 0 $ Point3f (-0.5) 0 0
      let p' = Plane 1 0 0 $ Point3f 0 0 0
      let p'' = Plane 1 0 0 $ Point3f 0.5 0 0
      cutFace 0 f p `shouldBe` ( Face center
                                      [ Point3f (-0.5) 0.5 0
                                      , Point3f (-1) 0 0
                                      , Point3f (-0.5) (-0.5) 0]
                                      [0]
                               , [ Point3f (-0.5) 0.5 0
                                 , Point3f (-0.5) (-0.5) 0])
      cutFace 0 f p' `shouldBe` ( Face center
                                       [ Point3f 0 1 0
                                       , Point3f (-1) 0 0
                                       , Point3f 0 (-1) 0]
                                       [0]
                                , [ Point3f 0 1 0
                                  , Point3f 0 (-1) 0])
      cutFace 0 f p'' `shouldBe` ( Face center
                                        [ Point3f 0.5 0.5 0
                                        , Point3f 0 1.0 0
                                        , Point3f (-1.0) 0 0
                                        , Point3f 0 (-1.0) 0
                                        , Point3f 0.5 (-0.5) 0]
                                        [0]
                                  , [ Point3f 0.5 0.5 0
                                    , Point3f 0.5 (-0.5) 0])
