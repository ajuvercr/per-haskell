module Main where

import Lib
import Graph

main :: IO ()
main = do
    print $ topoDfs topo 0
    print $ topoBfs topo 0
    where
        topo = buildTopo 0 5 `addEdge` (0, 1) `addEdge` (0, 2) `addEdge` (1, 2) `addEdge` (1, 3) `addEdge` (2, 3) `addEdge` (3, 2)
