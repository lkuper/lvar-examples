{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Control.LVish
import Control.LVish.DeepFrz
import Data.LVar.Generic (addHandler, freeze)
import Data.LVar.PureSet
import qualified Data.Graph as G

-- A toy graph for demonstration purposes.
myGraph = first $ G.graphFromEdges [("node0", 0, [1, 6, 7]),
                                    ("node1", 1, [4]),
                                    ("node2", 2, [1]),
                                    ("node3", 3, []),
                                    ("node4", 4, [3, 5]),
                                    ("node5", 5, [3]),
                                    ("node6", 6, [10]),
                                    ("node7", 7, []),
                                    ("node8", 8, []),
                                    ("node9", 9, [4, 11]),
                                    ("node10", 10, [9]),
                                    ("node11", 11, [10])]

-- Argh!  Why isn't this built in?!
first (x, _, _) = x

-- Takes a graph and a vertex and returns a list of the vertex's
-- neighbors.  This seems silly, but it's the first thing that comes
-- to mind given the Data.Graph API.
neighbors :: G.Graph -> G.Vertex -> [G.Vertex]
neighbors g v =
  map snd edgesFromNode where
    edgesFromNode = filter (\(v1, _) -> v1 == v) (G.edges g)

traverse :: (HasPut e, HasGet e, HasFreeze e) => G.Graph -> Int -> Par e s (ISet Frzn Int)
traverse g startNode = do
  seen <- newEmptySet
  h <- newHandler seen
       (\node -> do
           mapM (\v -> insert v seen)
             (neighbors g node)
           return ())
  insert startNode seen -- Kick things off
  quiesce h
  freeze seen

newHandler seen f = do
  hp <- newPool
  addHandler (Just hp) seen f
  return hp

main = do
  v <- runParQuasiDet $ traverse myGraph (0 :: G.Vertex)
  print $ fromISet v
