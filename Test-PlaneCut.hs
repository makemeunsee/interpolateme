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

  -- TODO no formal solution yet. work around is to lower tolerance. Or reject the cut, as coplanar to an existing face.
  describe "numeric approximation bug" $ do
    it "should cut" $ do
      let p0 = (0,(Point3f 0 0.1 0.1,[0,1,4]))
      let p1 = (1,(Point3f 10 1 1,[0,1,2]))
      let p2 = (2,(Point3f 10 (-1) (-1),[0,2,3]))
      let p3 = (3,(Point3f 0 (-0.1) (-0.1),[0,3,4]))
      let pts = [p0,p1,p2,p3]
      let f0 = ([0,1,2,3], 0)
      let f1 = ([0,1], 1)
      let f2 = ([1,2], 2)
      let f3 = ([2,3], 3)
      let f4 = ([0,3], 4)
      let fs = [f0,f1,f2,f3,f4]
      let ns = take 5 $ zip (repeat $ normalized $ Point3f 0 1 (-1)) [0..]
      let fm = FacedModel pts fs ns

      let tolerance = 0.1

      let FacedModel pts' fs' ns' = cutModel tolerance (Plane 0 1 0 $ Point3f 5 0 0) fm
      pts' `shouldBe` [(2,(Point3f 10.0 (-1.0) (-1.0),[0,2,3])),(3,(Point3f 0.0 (-0.1) (-0.1),[0,3,4])),(5,(Point3f 10.0 0.0 0.0,[5,0,2])),(4,(Point3f 0.0 0.0 0.0,[5,0,4]))]
      fs' `shouldBe` [([4,5],5),([5,2,3,4],0),([2,5],2),([3,2],3),([3,4],4)]