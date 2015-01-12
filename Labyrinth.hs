module Labyrinth

where

import qualified Geometry as G
import qualified BinaryTree as BT
import ListUtil


-- a simple data structure for a graph / labyrinth.
-- each node/leaf should hold a unique index value
data Labyrinth = Node Int [Labyrinth]
               | Leaf Int
               deriving (Eq, Show)


-- the cell index held at the head of this labyrinth
value :: Labyrinth -> Int
value (Leaf i) = i
value (Node i _) = i


-- the number of elements in this labyrinth
size :: Labyrinth -> Int
size (Leaf _) = 1
size (Node _ ls) = 1 + foldr (\l s -> s + size l) 0 ls


-- topology: a list of connections between nodes of a graph
-- [[1],[0]] -> 2 nodes, connected to each other
-- [[1,2],[0],[0]] -> 3 nodes, the first one connected to the 2 others
-- topology MUST be consistent
topologyToLabyrinth :: [[Int]] -> Labyrinth
topologyToLabyrinth [] = Leaf (-1) -- illegal
topologyToLabyrinth ls = fst $ topologyToLabyrinth0 (BT.BTNode 0 Nothing Nothing) ls 0
  where
    topologyToLabyrinth0 visited ls i =
      let r = filter (\j -> not $ BT.elem j visited) $ ls !! i in
      if r == [] then
        (Leaf i, visited)
      else
        let (subnodes, visited') = foldr (\j (newNodes, oldVisited) ->
                                           if BT.elem j oldVisited then
                                             (newNodes, oldVisited)
                                           else
                                             let (newNode, newVisited) = topologyToLabyrinth0 (BT.insert j oldVisited) ls j in
                                             (newNode : newNodes, newVisited)
                                         ) ([], visited) r in
        (Node i subnodes, visited')


-- given a solid (vertice and faces) and a labyrinth which values are face indice,
-- build the 3D vertex data representing this labyrinth
-- labyrinth MUST be consistent with solid topology
labyrinthToPathVertice :: RealFloat a => [G.Point3f a] -> [[Int]] -> Labyrinth -> [G.Point3f a]
labyrinthToPathVertice vertice faces (Leaf i) = [bary]
  where
    face = faces !! i
    bary = G.faceBarycenter vertice face
labyrinthToPathVertice vertice faces (Node i ls) = bary : concatMap (uncurry prependBaryCenter) subnodesVerts
  where
    face = faces !! i
    bary = G.faceBarycenter vertice face
    subnodesVerts = map (\l -> (labyrinthToPathVertice vertice faces l, value l)) ls
    prependBaryCenter vs j = (junctionPoint j) : vs
    junctionPoint j =
      let [i0, i1] = intersection (faces !! i) (faces !! j) in
      G.times 0.5 $ G.add (vertice !! i0) (vertice !! i1)


-- build a list of indice defining segments, to be used along labyrinthToVertice
labyrinthToPathIndice :: Integral a => a -> Labyrinth -> [a]
labyrinthToPathIndice offset (Leaf _) = []
labyrinthToPathIndice offset (Node _ ls) = indice
  where
    (_, indice) = foldl (\(o,ids) l -> (o+(fromIntegral $ 2*size l), offset : o+1 : o+1 : o+2 : labyrinthToPathIndice (o+2) l ++ ids)) (offset, []) ls


labyrinthToWallVertice :: RealFloat a => [G.Point3f a] -> [[Int]] -> Labyrinth -> [(Int,Int)] -> [G.Point3f a]
labyrinthToWallVertice vertice faces (Leaf i) parentEdges = labyrinthToWallVertice vertice faces (Node i []) parentEdges
labyrinthToWallVertice vertice faces (Node i ls) parentEdges = ownWalls ++ concatMap (\n -> labyrinthToWallVertice vertice faces n edges) ls
  where
    edges = cyclicConsecutivePairs $ faces !! i
    childEdges = concatMap (\l -> cyclicConsecutivePairs $ faces !! value l) ls
    toAvoid = parentEdges ++ childEdges
    ownWalls = concatMap (edgeToWall vertice) $ filter (\(j,k) -> not $ elem (k,j) toAvoid) edges


edgeToWall vertice (i,j) = [vertice !! i, vertice !! j]


labyrinthToWallIndice :: Integral a => a -> [[Int]] -> Labyrinth -> ([a], a)
labyrinthToWallIndice offset faces (Leaf x) = labyrinthToWallIndice offset faces (Node x [])
labyrinthToWallIndice offset faces (Node i ls) = (indice, newOffset)
  where
    wallCount = length (faces !! i) - length ls
    newOffset0 = offset + (fromIntegral wallCount) * 2
    ownIndice = concatMap (\j -> map (j*2 + offset + ) [0,1]) $ take wallCount [0..]
    (indice, newOffset) = foldl (\(ids, o) l ->
                                  let (newIds, o') = labyrinthToWallIndice o faces l in
                                  (ids ++ newIds, o')
                                )
                                (ownIndice, newOffset0)
                                ls