module Labyrinth

where

import qualified Data.List as L
import Data.Set (singleton, notMember, insert)
import qualified Data.Sequence as S

import qualified Geometry as G
import ListUtil
import VoronoiCut

-- a simple data structure for a graph / labyrinth.
-- each node/leaf should hold a unique index value
data Labyrinth a = Node a ![Labyrinth a]
                 | Leaf a
                 deriving (Eq, Show)


-- the cell index held at the head of this labyrinth
value :: Labyrinth a -> a
value (Leaf i) = i
value (Node i _) = i


values :: Labyrinth a -> [a]
values (Leaf i) = [i]
values (Node i ls) = i : concatMap values ls


-- the number of elements in this labyrinth
size :: Labyrinth a -> Int
size (Leaf _) = 1
size (Node _ ls) = 1 + foldr (\l s -> s + size l) 0 ls


longestBranch :: Labyrinth a -> Int
longestBranch (Leaf _) = 1
longestBranch (Node _ ls) = 1 + foldr (\l m -> max m $ longestBranch l) 0 ls


depthMap :: Labyrinth a -> [(a, Int)]
depthMap = depthMap0 0
  where depthMap0 :: Int -> Labyrinth a -> [(a, Int)]
        depthMap0 offset (Leaf i) = [(i, offset)]
        depthMap0 offset (Node i ls) = (i, offset) : concatMap (depthMap0 (offset+1)) ls


elem :: Eq a => a -> Labyrinth a -> Bool
elem k (Leaf i) = k == i
elem k (Node j ls) = k == j || any (Labyrinth.elem k) ls


-- blindly insert a leaf of value 'k' into the labyrinth, at node of value 'at', if any
-- does NOT check for duplicates
insertAt :: Eq a => a -> a -> Labyrinth a -> Labyrinth a
insertAt k at laby = case laby of
  Leaf i | i == at   -> Node i [Leaf k]                      -- turn leaf into a parent node
         | otherwise -> Leaf i                               -- return unconcerned leaf intact
  Node i ls | i == at   -> Node i $ (Leaf k) : ls            -- insert k as new leaf to found parent
            | otherwise -> Node i $ map (insertAt k at) ls   -- explore the labyrinth to find the right parent


labyrinth2 :: S.Seq (Face a) -> Labyrinth Int
labyrinth2 topo
  | S.null topo = Leaf (-1) -- illegal
  | otherwise   = laby2Rec [0] (Leaf 0) (singleton 0)
  where
    laby2Rec [] laby visited = laby
    laby2Rec stack laby visited = let currentId = head stack in
                          let currentNeighbours = neighbours $ S.index topo currentId in
                          case findExplorable currentId currentNeighbours visited of
                            Just i        -> laby2Rec (i : stack) (insertAt i currentId laby) (insert i visited)
                            Nothing       -> laby2Rec (tail stack) laby visited
    findExplorable i ids visited = L.find (explorable i visited) ids
    -- a node (face) is explorable...
    explorable parentId visited i  =
      -- if it's not part of the maze already
      notMember i visited &&
      -- and the faces it touches are either the parent face or not in the maze
      (all (\j -> j == parentId || notMember j visited)
           $ neighbours $ S.index topo i)


-- topology: a list of connections between nodes of a graph
-- [[1],[0]] -> 2 nodes, connected to each other
-- [[1,2],[0],[0]] -> 3 nodes, the first one connected to the 2 others
-- topology MUST be consistent
labyrinth1 :: S.Seq (Face a) -> Labyrinth Int
labyrinth1 topo
  | S.null topo = Leaf (-1) -- illegal
  | otherwise   = fst $ topologyToLabyrinth0 (singleton 0) 0
  where
    topologyToLabyrinth0 visited i =
      let (subnodes, visited') = foldr (\j (newNodes, oldVisited) ->
                                         if notMember j oldVisited then
                                           let (newNode, newVisited) = topologyToLabyrinth0 (insert j oldVisited) j in
                                           (newNode : newNodes, newVisited)
                                         else
                                           (newNodes, oldVisited)
                                       )
                                       ([], visited)
                                       $ neighbours $ S.index topo i in
      if subnodes == [] then
        (Leaf i, visited')
      else
        (Node i subnodes, visited')


-- given a solid (vertice and faces) and a labyrinth which values are face indice,
-- build the 3D vertex data representing this labyrinth
-- labyrinth MUST be consistent with solid topology
labyrinthToPathVertice :: RealFloat a => S.Seq (Face a) -> Labyrinth Int -> [G.Point3f a]
labyrinthToPathVertice faces (Leaf i) = [bary]
  where
    face = S.index faces i
    bary = barycenter face
labyrinthToPathVertice faces (Node i ls) = bary : concatMap (uncurry prependBaryCenter) subnodesVerts
  where
    face = S.index faces i
    bary = barycenter face
    subnodesVerts = map (\l -> (labyrinthToPathVertice faces l, value l)) ls
    prependBaryCenter vs j = (junctionPoint j) : vs
    junctionPoint j =
      let [v0, v1] = intersection (vertice face) (vertice $ S.index faces j) in
      G.times 0.5 $ G.add v0 v1


-- build a list of indice defining segments, to be used along labyrinthToVertice
labyrinthToPathIndice :: Integral a => a -> Labyrinth b -> [a]
labyrinthToPathIndice offset (Leaf _) = []
labyrinthToPathIndice offset (Node _ ls) = indice
  where
    (_, indice) = foldl (\(o,ids) l -> (o+(fromIntegral $ 2*size l), offset : o+1 : o+1 : o+2 : labyrinthToPathIndice (o+2) l ++ ids)) (offset, []) ls


labyrinthToWallVertice :: RealFloat a => S.Seq (Face a) -> Labyrinth Int -> [(G.Point3f a, G.Point3f a)] -> [G.Point3f a]
labyrinthToWallVertice faces (Leaf i) parentEdges = labyrinthToWallVertice faces (Node i []) parentEdges
labyrinthToWallVertice faces (Node i ls) parentEdges = ownWalls ++ concatMap (\n -> labyrinthToWallVertice faces n edges) ls
  where
    edges = cyclicConsecutivePairs $ vertice $ S.index faces i
    childEdges = concatMap (\l -> cyclicConsecutivePairs $ vertice $ S.index faces $ value l) ls
    toAvoid = parentEdges ++ childEdges
    ownWalls = concatMap (\(i,j) -> [i,j]) $ filter (\(j,k) -> not $ L.elem (k,j) toAvoid) edges


labyrinthToWallIndice :: Integral a => a -> [[Int]] -> Labyrinth Int -> ([a], a)
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