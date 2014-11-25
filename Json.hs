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
import Geometry (Point3f (Point3f), Model (Model), modelAutoNormals, combine, barycenter, times, add, rotateL)


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


data Mesh = Mesh { vertice :: [CFloat], faces :: [[Int]], normals :: Maybe [CFloat] }
            deriving (Show)

instance FromJSON Mesh where
  parseJSON (Object o) = Mesh <$> o .: "vertices" <*> o .: "faces" <*> o .:? "normals"
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


transformOne :: MeshList -> Int -> [Model CFloat]
transformOne (MeshList rt ms) meshId =
  map toModel all
  where Mesh vs fs ns = ms !! meshId
        asP3f floats = map (\[x,y,z] -> Point3f x y z) $ chop 3 floats

        toModel t = case ns of
          Nothing     -> modelAutoNormals (t $ asP3f vs) fs
          Just floats -> Model (t $ asP3f vs) fs (t $ asP3f floats)

        -- apply transform = rotate converting given list to rot matrix
        applyTransform t = map (rotateL t)

        -- extract root transform
        rootTransform = applyTransform $ transformation rt

        -- each child transformation is applied to the root transformed mesh,
        -- creating a new mesh for each child transformation
        kids = maybe [] id (children rt)
        relevantKids = filter (\ k -> elem meshId $ maybe [] id (meshIds k)) kids
        kidTransforms = map cTransformation relevantKids
        all = if kidTransforms == [] then [rootTransform]
                                     else map (\ t -> rootTransform . applyTransform t ) kidTransforms


parseJson :: [Int] -> MeshList -> Model CFloat
parseJson indice ml = Model centered fs ns
  where ids = if indice == [] then allMeshIds else indice
        allMeshIds = take (length $ meshes ml) [0..]
        Model vs fs ns = foldr1 combine $ concatMap (transformOne ml) ids
        bary = barycenter vs
        centered = map ((-1) `times` bary `add`) vs