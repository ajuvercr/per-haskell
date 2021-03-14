
{-# LANGUAGE TupleSections #-}
module Graph where

import GHC.Arr ( Array )
import qualified GHC.Arr as A
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Ix
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import qualified Data.Set as Set

import qualified Control.Monad.State.Lazy as St
import Control.Monad
import Data.Functor
import Debug.Trace
import Util (if')
import Control.Applicative ((<|>))

data Graph i v e = Graph
    { size     :: (i, i)
    , edges    :: Array (i, i) (Maybe e)
    , nodes    :: Array i      v
    , nodesInv :: Map   v      i
    } deriving (Show)


transpose2 :: ((a1, a2), (b1, b2)) -> ((a1, b1), (a2, b2))
transpose2 ((a, b), (c,d)) = ((a,c), (b,d))

buildGraph :: (Ix i, Ord v) => (i, i) -> [v] -> Graph i v e
buildGraph size vs = Graph size (A.listArray (transpose2 (size, size)) (repeat Nothing)) (A.listArray size vs) $ M.fromList (zip vs (range size))

leaving :: (Ix i, Ord v) => Graph i v e -> v -> [(e, i, v)]
leaving graph v = leaving' graph $ nodesInv graph M.! v

leaving' :: Ix i => Graph i v e -> i -> [(e, i, v)]
leaving' Graph {edges=edges, size=size, nodes=nodes} i = mapMaybe f [(i, j) | j <- range size]
    where f (i,j) = (,j,nodes A.! j) <$> edges A.! (i,j)


entering :: (Ix i, Ord v) => Graph i v e -> v -> [(e, i, v)]
entering graph v = entering' graph $ nodesInv graph M.! v

entering' :: Ix i => Graph i v e -> i -> [(e, i, v)]
entering' Graph {edges=edges, size=size, nodes=nodes} j = mapMaybe f [(i, j) | i <- range size]
    where f (i,j) = (,i,nodes A.! i) <$> edges A.! (i,j)



type GraphTopo i = Array (i, i) Bool

buildTopo :: Ix i => i -> i -> GraphTopo i
buildTopo start end = A.listArray ((start, start), (end, end)) (repeat False)


addEdge :: Ix i => GraphTopo i -> (i, i) -> GraphTopo i
addEdge topo s = topo A.// [(s, True)]

removeEdge :: Ix i => GraphTopo i -> (i, i) -> GraphTopo i
removeEdge topo s = topo A.// [(s, False)]


topoLeaving :: Ix i => GraphTopo i -> i -> [(i, i)]
topoLeaving topo start = filter (topo A.!) [(start, e) | e <- range (s, e)]
    where
        ((s, _), (e, _)) = A.bounds topo


topoEntering :: Ix i => GraphTopo i -> i -> Seq i
topoEntering topo end = Seq.fromList $ map fst $ filter (topo A.!) [(s, end) | s <- range bounds]
    where
        (bounds, _) = A.bounds topo


topoBfs :: (Show i, Ix i) => GraphTopo i -> i -> Seq (i, i)
topoBfs topo start = St.evalState (bfs topo $ Seq.singleton start) Set.empty
    where
        bfs :: (Show i, Ix i) => GraphTopo i -> Seq i -> St.State (Set.Set i) (Seq (i, i))
        bfs topo Seq.Empty = return Seq.Empty
        bfs topo (start Seq.:<| xs) = do
            done <- St.get
            let children = filter (\s-> not $ Set.member s done) $ map snd $ topoLeaving topo start
                outs = Seq.fromList $ map (start,) children
            St.modify (Set.union (Set.fromList children))
            rest <- bfs topo (xs Seq.>< Seq.fromList children)
            return $ outs Seq.>< rest



topoDfs :: (Show i, Ix i) => GraphTopo i -> i -> Seq (i, i)
topoDfs topo start = St.evalState (dfs topo start) Set.empty
    where
        dfs :: (Show i, Ix i) => GraphTopo i -> i -> St.State (Set.Set i) (Seq (i, i))
        dfs topo start = do
            done <- Set.member start <$> St.get
            if done
            then return Seq.empty
            else do
                St.modify (Set.insert start)
                let children = map snd $ topoLeaving topo start
                    f cum child = (Seq.>< cum) <$> dfs' topo start child
                foldM f Seq.empty children

        dfs' :: (Show i, Ix i) => GraphTopo i -> i -> i -> St.State (Set.Set i) (Seq (i, i))
        dfs' topo start end = do
            done <- Set.member end <$> St.get
            if done
            then dfs topo end
            else ((start, end) Seq.<|) <$> dfs topo end

        step :: (Ix i) => GraphTopo i -> i -> Seq (i, i)
        step topo start = Seq.fromList $ topoLeaving topo start


topoDfs' :: (Show i, Ix i) => GraphTopo i -> i -> Seq (i, i)
topoDfs' topo start = St.evalState (dfs topo start) Set.empty
    where
        dfs :: (Show i, Ix i) => GraphTopo i -> i -> St.State (Set.Set i) (Seq (i, i))
        dfs topo start = do
            St.modify (Set.insert start)
            let children = map snd $ topoLeaving topo start
                f cum child = do
                    done <- Set.member child <$> St.get
                    rest <- if' done (return Seq.empty) $ ((start, child) Seq.<|) <$> dfs topo child
                    return $ rest Seq.>< cum
            foldM f Seq.empty children


topoPath :: (Show i, Ix i) => GraphTopo i -> i -> i -> Maybe [i]
topoPath topo start end = St.evalState (path topo start end) Set.empty
    where
        path :: (Show i, Ix i) => GraphTopo i -> i -> i -> St.State (Set.Set i) (Maybe [i])
        path topo start end | start == end = return $ Just [start]
        path topo start end = do
            St.modify (Set.insert start)
            let children = map snd $ topoLeaving topo start
                f child = do
                    done <- Set.member child <$> St.get
                    if' done (return Nothing) $ ((start:) <$>) <$> path topo child end
            foldl1 (<|>) <$> mapM f children
