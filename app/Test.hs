{-# LANGUAGE TupleSections #-}

module Main where

import Lib
import Graph
import qualified Data.Map as M

main :: IO ()
main = do
    -- print $ topoDfs' topo 0
    -- print $ topoDfs topo 0
    -- print $ topoBfs topo 0
    print $ topoPath (map (,id) . topoLeaving' topo) 0 4
    putStr "Entering 2: "
    print $ topoEntering topo 2
    putStr "Leaving 2: "
    print $ topoLeaving topo 2
    print $ maxFlow' flow 0 3
    where
        topo = buildTopo 0 5 `addEdge` (0, 1) `addEdge` (0, 2) `addEdge` (1, 3) `addEdge` (2, 3) `addEdge` (3, 2) `addEdge` (2, 4)

        flow = M.fromList [((0, 1), 10), ((0, 2), 5), ((1,2), 15), ((1, 3), 5), ((2, 3), 10)]
