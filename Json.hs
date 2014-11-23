{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Json ( readJson
            , parseJson
            )
where

import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)

import FloretSphere
import ListUtil
import Geometry (Point3f (Point3f))

newtype MeshList = MeshList { meshes :: [Mesh] }
        deriving (Show)

instance FromJSON MeshList where
  parseJSON (Object o) = MeshList <$> (o .: "meshes")
  parseJSON _ = mzero

data Mesh = Mesh { name :: String, vertice :: [Float], faces :: [[Int]] }
            deriving (Show)

instance FromJSON Mesh where
  parseJSON (Object o) = Mesh <$> o .: "name" <*> o .: "vertices" <*> o .: "faces"
  parseJSON _ = mzero



readJson :: Maybe FilePath -> IO (Maybe MeshList)
readJson Nothing = return Nothing
readJson (Just path) = do
  content <- readFile path
  return (decode (BL.pack content) :: Maybe MeshList)


parseJson :: MeshList -> Polyhedron
parseJson ms = Polyhedron v (Json.faces m)
  where m = head $ meshes ms
        v = map (\[x,y,z] -> Point3f x y z) $ chop 3 $ Json.vertice m