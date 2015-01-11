module Labyrinth

where

import qualified Geometry as G
import ListUtil


-- a simple data structure representating of a labyrinth for a graph of cells indexed by ints
data Labyrinth = Node Int [Labyrinth]
               | Leaf Int


-- the cell index held at the head of this labyrinth
value :: Labyrinth -> Int
value (Leaf i) = i
value (Node i _) = i


-- the number of elements in this labyrinth
size :: Labyrinth -> Int
size (Leaf _) = 1
size (Node _ ls) = 1 + foldr (\l s -> s + size l) 0 ls


-- given a solid (vertice and faces) and a labyrinth which values are face indice,
-- build the 3D vertex data representing this labyrinth
-- labyrinth MUST be consistent with solid topology
labyrinthToVertice :: RealFloat a => [G.Point3f a] -> [[Int]] -> Labyrinth -> [G.Point3f a]
labyrinthToVertice vertice faces (Leaf i) = [bary]
  where
    face = faces !! i
    bary = G.faceBarycenter vertice face
labyrinthToVertice vertice faces (Node i ls) = bary : concatMap (uncurry prependBaryCenter) subnodesVerts
  where
    face = faces !! i
    bary = G.faceBarycenter vertice face
    subnodesVerts = map (\l -> (labyrinthToVertice vertice faces l, value l)) ls
    prependBaryCenter vs j = (junctionPoint j) : vs
    junctionPoint j =
      let [i0, i1] = intersection (faces !! i) (faces !! j) in
      G.times 0.5 $ G.add (vertice !! i0) (vertice !! i1)


-- build a list of indice defining segments, to be used along labyrinthToVertice
labyrinthToIndice :: Integral a => a -> Labyrinth -> [a]
labyrinthToIndice offset (Leaf _) = []
labyrinthToIndice offset (Node _ ls) = indice
  where
    (_, indice) = foldl (\(o,ids) l -> (o+(fromIntegral $ 2*size l), offset : o+1 : o+1 : o+2 : labyrinthToIndice (o+2) l ++ ids)) (offset, []) ls