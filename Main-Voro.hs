{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (
    main
) where

import qualified VoronoiCut as VC
import FloretSphere
import qualified Geometry as G

main :: IO ()
main = do
  let m = cube
  let vm = VC.fromModel m
  let m' = VC.toModel vm
  putStrLn $ show m
  putStrLn $ show vm
--  putStrLn $ show m'
  putStrLn $ show $ VC.closestFaceCenter vm $ G.normalized $ G.Point3f (-0.1) (-1.1) 0