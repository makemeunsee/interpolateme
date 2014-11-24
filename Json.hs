{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Json ( readJson
            , parseJson
            )
where

import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)
import Data.Maybe

import Foreign.C.Types (CFloat)
import Data.Scientific (Scientific)
import qualified Data.Scientific as Scientific (toRealFloat)

import FloretSphere
import ListUtil
import Geometry (Point3f (Point3f), Model (Model), combine, barycenter, times, add, rotateL)


-- parseJSON instance for CFloat/GLfloat
instance FromJSON CFloat  where
  parseJSON = parseRealFloat "CFloat"
  {-# INLINE parseJSON #-}


parseRealFloat :: RealFloat a => String -> Value -> Parser a
parseRealFloat _ (Number s) = pure $ Scientific.toRealFloat s
parseRealFloat _ Null = pure (0/0)
parseRealFloat expected v = typeMismatch expected v


-- assimp json partial structure

data Child = Child { cTransformation :: [CFloat], meshIds :: Maybe [Int] }
             deriving (Show)

instance FromJSON Child where
  parseJSON (Object o) = Child <$> o .: "transformation" <*> o .:? "meshes"
  parseJSON _ = mzero


data RootNode = RootNode { transformation :: [CFloat], children :: Maybe [Child] }
                deriving (Show)

instance FromJSON RootNode where
  parseJSON (Object o) = RootNode <$> o .: "transformation" <*> o .:? "children"
  parseJSON _ = mzero


data MeshList = MeshList { rootnode :: RootNode, meshes :: [Mesh] }
                deriving (Show)

instance FromJSON MeshList where
  parseJSON (Object o) = MeshList <$> o .: "rootnode" <*> o .: "meshes"
  parseJSON _ = mzero


data Mesh = Mesh { vertice :: [CFloat], faces :: [[Int]] }
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


transformOne :: MeshList -> Int -> Model CFloat
transformOne (MeshList rt ms) meshId =
  Model transformed fs
  where Mesh vs fs = ms !! meshId
        transformed = map (\[x,y,z] -> transform $ Point3f x y z) $ chop 3 vs
        tRoot = transformation rt
        transform = rotateL tRoot
        -- unsure how to apply mesh specific transformations yet
--        tChild = map cTransformation $ filter (\c -> contains meshId $ meshIds c) $ children rt
--        transform = foldl (\f t -> f . (rotateL t)) (rotateL tRoot) tChild
--        tChild = listToMaybe $ map cTransformation $ filter (\c -> contains meshId $ meshIds c) $ children rt
--        transform = rotateL tRoot . maybe id (\t -> rotateL t) tChild


parseJson :: Maybe [Int] -> MeshList -> Model CFloat
parseJson indice ml = Model centered fs
  where ids = maybe allMeshIds id indice
        allMeshIds = take (length $ meshes ml) [0..]
        Model vs fs = foldr1 combine $ map (transformOne ml) ids
        bary = barycenter vs
        centered = map ((-1) `times` bary `add`) vs