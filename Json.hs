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
import Data.Maybe

import FloretSphere
import ListUtil
import Geometry (Point3f (Point3f), Model (Model), barycenter, times, add, rotateL)


data Child = Child { cTransformation :: [Float], meshIds :: [Int] }
             deriving (Show)

instance FromJSON Child where
  parseJSON (Object o) = Child <$> o .: "transformation" <*> o .: "meshes"
  parseJSON _ = mzero


data RootNode = RootNode { transformation :: [Float], children :: [Child] }
                deriving (Show)

instance FromJSON RootNode where
  parseJSON (Object o) = RootNode <$> o .: "transformation" <*> o .: "children"
  parseJSON _ = mzero


data MeshList = MeshList { rootnode :: RootNode, meshes :: [Mesh] }
                deriving (Show)

instance FromJSON MeshList where
  parseJSON (Object o) = MeshList <$> o .: "rootnode" <*> o .: "meshes"
  parseJSON _ = mzero


data Mesh = Mesh { vertice :: [Float], faces :: [[Int]] }
            deriving (Show)

instance FromJSON Mesh where
  parseJSON (Object o) = Mesh <$> o .: "vertices" <*> o .: "faces"
  parseJSON _ = mzero


readJson :: Maybe FilePath -> IO (Maybe MeshList)
readJson Nothing = return Nothing
readJson (Just path) = do
  content <- readFile path
  let res = eitherDecode (BL.pack content)
  case res of
    Right r -> return $ Just r
    Left err -> do putStrLn err
                   return Nothing


transformOne :: MeshList -> Int -> Model
transformOne (MeshList rt ms) meshId =
  Model transformed fs
  where Mesh vs fs = ms !! meshId
        transformed = map (\[x,y,z] -> transform $ Point3f x y z) $ chop 3 vs
        tRoot = transformation rt
        transform = rotateL tRoot
--        tChild = map cTransformation $ filter (\c -> contains meshId $ meshIds c) $ children rt
--        transform = foldl (\f t -> f . (rotateL t)) (rotateL tRoot) tChild
--        tChild = listToMaybe $ map cTransformation $ filter (\c -> contains meshId $ meshIds c) $ children rt
--        transform = rotateL tRoot . maybe id (\t -> rotateL t) tChild


parseJson :: MeshList -> Model
parseJson ml = Model centered fs
  where Model vs fs = transformOne ml 0
        bary = barycenter vs
        centered = map ((-1) `times` bary `add`) vs