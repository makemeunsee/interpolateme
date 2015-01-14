module Labyrinth

where

import qualified Data.List as L

import qualified Geometry as G
import qualified BinaryTree as BT
import ListUtil


-- a simple data structure for a graph / labyrinth.
-- each node/leaf should hold a unique index value
data Labyrinth a = Node a [Labyrinth a]
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


labyrinth2 :: [[Int]] -> Labyrinth Int
labyrinth2 [] = Leaf (-1) -- illegal
labyrinth2 topo = laby2Rec [0] (Leaf 0)
  where
    laby2Rec [] laby = laby
    laby2Rec stack laby = let currentId = head stack in
                          let currentNeighbours = topo !! currentId in
                          case findExplorable currentId currentNeighbours laby of
                            Just i        -> laby2Rec (i : stack) (insertAt i currentId laby)
                            Nothing       -> laby2Rec (tail stack) laby
    findExplorable i ids laby = L.find (explorable i laby) ids
    -- a node (face) is explorable...
    explorable parentId laby i  =
      -- if it's not part of the maze already
      not (Labyrinth.elem i laby) &&
      -- and the faces it touches are either the parent face or not in the maze
      all (\j -> j == parentId || not (Labyrinth.elem j laby))
          (topo !! i)


-- topology: a list of connections between nodes of a graph
-- [[1],[0]] -> 2 nodes, connected to each other
-- [[1,2],[0],[0]] -> 3 nodes, the first one connected to the 2 others
-- topology MUST be consistent
labyrinth1 :: [[Int]] -> Labyrinth Int
labyrinth1 [] = Leaf (-1) -- illegal
labyrinth1 ls = fst $ topologyToLabyrinth0 (BT.BTNode 0 Nothing Nothing) ls 0
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
labyrinthToPathVertice :: RealFloat a => [G.Point3f a] -> [[Int]] -> Labyrinth Int -> [G.Point3f a]
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
labyrinthToPathIndice :: Integral a => a -> Labyrinth b -> [a]
labyrinthToPathIndice offset (Leaf _) = []
labyrinthToPathIndice offset (Node _ ls) = indice
  where
    (_, indice) = foldl (\(o,ids) l -> (o+(fromIntegral $ 2*size l), offset : o+1 : o+1 : o+2 : labyrinthToPathIndice (o+2) l ++ ids)) (offset, []) ls


labyrinthToWallVertice :: RealFloat a => [G.Point3f a] -> [[Int]] -> Labyrinth Int -> [(Int,Int)] -> [G.Point3f a]
labyrinthToWallVertice vertice faces (Leaf i) parentEdges = labyrinthToWallVertice vertice faces (Node i []) parentEdges
labyrinthToWallVertice vertice faces (Node i ls) parentEdges = ownWalls ++ concatMap (\n -> labyrinthToWallVertice vertice faces n edges) ls
  where
    edges = cyclicConsecutivePairs $ faces !! i
    childEdges = concatMap (\l -> cyclicConsecutivePairs $ faces !! value l) ls
    toAvoid = parentEdges ++ childEdges
    ownWalls = concatMap (edgeToWall vertice) $ filter (\(j,k) -> not $ L.elem (k,j) toAvoid) edges


edgeToWall vertice (i,j) = [vertice !! i, vertice !! j]


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