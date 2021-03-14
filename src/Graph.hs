
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


type GraphTopo i = Array (i, i) Bool

buildTopo :: Ix i => i -> i -> GraphTopo i
buildTopo start end = A.listArray ((start, start), (end, end)) (repeat False)


addEdge :: Ix i => GraphTopo i -> (i, i) -> GraphTopo i
addEdge topo s = topo A.// [(s, True)]

addEdges :: Ix i => GraphTopo i -> [(i, i)] -> GraphTopo i
addEdges topo ss = topo A.// map (,True) ss

removeEdge :: Ix i => GraphTopo i -> (i, i) -> GraphTopo i
removeEdge topo s = topo A.// [(s, False)]

removeEdges :: Ix i => GraphTopo i -> [(i, i)] -> GraphTopo i
removeEdges topo ss = topo A.// map (,False) ss


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
            foldl (<|>) Nothing <$> mapM f children


-- No float please thanks
type Flow i a = M.Map (i, i) a


maxFlow :: (Show i, Ix i) => GraphTopo i -> i -> i -> Flow i Int -> Flow i Int
maxFlow topo start end flow = M.map fst $ St.evalState (maxFlow'' start end (M.map(0,) flow)) topo
    where
        diffF :: (Ord i) => Flow i (Int, Int) -> (i, i) -> Int
        diffF flow ix = u - l
            where (l, u) = flow M.! ix

        maxF :: (Ord i) => Flow i (Int, Int) -> (i, i) -> Bool
        maxF flow ix = l == u
            where (l, u) = flow M.! ix

        maxFlow'' :: (Show i, Ix i) => i -> i -> Flow i (Int, Int) -> St.State (GraphTopo i) (Flow i (Int, Int))
        maxFlow'' start end flow = do
            topo <- St.get
            case topoPath topo start end of
                Nothing   -> return flow
                Just path -> do
                    let fp = zip path $ tail path
                        minDiff = minimum $ map (diffF flow) fp
                        flow' = M.mapWithKey (\k (l, u) -> if' (k `elem` fp) (l+minDiff, u) (l, u)) flow
                        f topo = removeEdges topo $ filter (maxF flow') fp
                    St.modify f
                    maxFlow'' start end flow'

maxFlow' :: (Show i, Ix i) => Flow i Int -> i -> i -> Flow i Int
maxFlow' flow start end = maxFlow topo start end flow
    where
        ks = M.keys flow
        min' = minimum $ map (uncurry min) ks
        max' = maximum $ map (uncurry max) ks
        topo = buildTopo min' max' `addEdges` ks
