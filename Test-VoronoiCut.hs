module Main where

import Test.Hspec

import VoronoiCut
import Geometry
import FloretSphere

main :: IO ()
main = hspec $ do
  describe "validating closestFaceCenter function" $ do
    it "find cube centers" $ do
      let m = cube
      let vm = fromModel m
      closestFaceCenter vm ( normalized $ Point3f (1.1) (-0.1) 0 ) `shouldBe` (2, Point3f 1 0 0)
      closestFaceCenter vm ( normalized $ Point3f (0.1) (0.9) 0 ) `shouldBe` (0, Point3f 0 1 0)
      closestFaceCenter vm ( normalized $ Point3f (-0.1) 0 1 ) `shouldBe` (4, Point3f 0 0 1)
      closestFaceCenter vm ( normalized $ Point3f (-0.9) 0 0 ) `shouldBe` (3, Point3f (-1) 0 0)
      closestFaceCenter vm ( normalized $ Point3f (-0.1) (-1.1) 0 ) `shouldBe` (1, Point3f 0 (-1) 0)
      closestFaceCenter vm ( normalized $ Point3f (0.1) 0 (-1.1) ) `shouldBe` (5, Point3f 0 0 (-1))