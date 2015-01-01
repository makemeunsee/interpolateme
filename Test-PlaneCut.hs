module Main where

import Test.Hspec

import PlaneCut
import Geometry

main :: IO ()
main = hspec $ do
  describe "validating chain function" $ do
    it "should not accept unchainable lists" $ do
      let p = Point3f 0 0 0

      let unchainedBugSimplified = [ ((p, [0,1,2]), 0)
                                   , ((p, [0,1,2]), 1)
                                   , ((p, [0,2,3]), 2)
                                   , ((p, [0,4,1]), 4)
                                   , ((p, [0,3,4]), 3)
                                   ]
      chain unchainedBugSimplified [] `shouldBe` Nothing

--      let unchainedBug = [ ((p,[149,23,15]),0)
--                         , ((p,[149,23,15]),1)
--                         , ((p,[149,35,1]),2)
--                         , ((p,[149,60,15]),3)
--                         , ((p,[149,97,60]),4)
--                         , ((p,[149,97,35]),5)
--                         , ((p,[149,129,23]),6)
--                         , ((p,[149,129,1]),7)]
--      chain unchainedBug [] `shouldBe` Nothing
--
--      let unchainedBug' = [ ((Point3f (-0.58830523) (-0.3445781) 0.79916763,[149,23,15]),297)
--                          , ((Point3f (-0.58830523) (-0.3445781) 0.79916763,[149,23,15]),296)
--                          , ((Point3f (-0.20047778) (-0.17686161) 0.9938907,[149,35,1]),295)
--                          , ((Point3f (-0.4753977) (-0.41337168) 0.84328157,[149,60,15]),294)
--                          , ((Point3f (-0.42957) (-0.4192081) 0.86354816,[149,97,60]),293)
--                          , ((Point3f (-0.20466718) (-0.2832958) 0.9806015,[149,97,35]),292)
--                          , ((Point3f (-0.6244454) (-9.031766e-2) 0.80987746,[149,129,23]),291)
--                          , ((Point3f (-0.44437754) (-8.311763e-3) 0.9007293,[149,129,1]),290)]
--      chain unchainedBug' [] `shouldBe` Nothing