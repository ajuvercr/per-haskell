
{-# LANGUAGE TupleSections, BangPatterns #-}
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
import Util (if', applyAll)
import Control.Applicative ((<|>))
import qualified Control.Arrow as Data.Bifunctor


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


topoLeaving' :: Ix i => GraphTopo i -> i -> [i]
topoLeaving' topo = map snd . topoLeaving topo


topoEntering :: Ix i => GraphTopo i -> i -> [(i, i)]
topoEntering topo end = filter (topo A.!) [(s, end) | s <- range (s, e)]
    where
        ((s, _), (e, _)) = A.bounds topo


topoEntering' :: Ix i => GraphTopo i -> i -> [i]
topoEntering' topo = map snd . topoEntering topo


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

topoPath :: (Show i, Ord i) => (i -> [(i, b)]) -> i -> i -> Maybe [b]
topoPath topo start end = St.evalState (path topo start end) Set.empty
    where
        path :: (Show i, Ord i) => (i -> [(i, b)]) -> i -> i -> St.State (Set.Set i) (Maybe [b])
        path topo start end | start == end = return $ Just []
        path topo start end = do
            St.modify (Set.insert start)
        -- let children = map snd $ topoLeaving topo start
            let children = topo start
                f (child, d) = do
                    done <- Set.member child <$> St.get
                    if' done (return Nothing) $ ((d:) <$>) <$> path topo child end
            foldl (<|>) Nothing <$> mapM f children


-- No float please thanks
type Flow i a = M.Map (i, i) a



maxFlow :: (Show i, Ix i) => GraphTopo i -> i -> i -> Flow i Int -> Flow i Int
maxFlow topo start end flow = M.map fst $ maxFlow'' topo start end (M.map(0,) flow)
    where
        maxFlow'' !topo !start !end !flow = case topoPath (leavingF topo flow) start end of
            Nothing -> flow
            -- [(key, max throughput, how to apply)]
            Just path -> maxFlow'' topo start end newFlow
                where
                    minDiff = minimum $ map (\(_, x, _) -> x) path
                    path' = map (\(key, _, f) -> (key, Data.Bifunctor.first (f minDiff))) path
                    newFlow = applyAll flow path'

        goodForward :: (Ord i) => Flow i (Int, Int) -> (i, i) -> Maybe (i, ((i, i), Int, Int -> Int -> Int))
        goodForward flow i
            -- (key, capacity - flow, apply plus)
            | l < u     = Just (snd i, (i, uncurry (flip (-)) $ flow M.! i, (+)))
            | otherwise = Nothing
            where (l, u) = flow M.! i

        goodBackwards :: (Ord i) => Flow i (Int, Int) -> (i, i) -> Maybe (i, ((i, i), Int, Int -> Int -> Int))
        goodBackwards flow (i, j)
            -- (inverted key 'back edge', current flow, apply minus)
            | l > 0     = Just (i, ((i, j), fst $ flow M.! (i,j), flip (-)))
            | otherwise = Nothing
            where (l, u) = flow M.! (i, j)

        leavingF :: (Show i, Ix i) => GraphTopo i -> Flow i (Int, Int) -> i -> [(i, ((i, i), Int, Int -> Int -> Int))]
        leavingF topo flow f = forward ++ backward
            where
                forward = mapMaybe (goodForward flow) (topoLeaving topo f)
                backward = mapMaybe (goodBackwards flow) (topoEntering topo f)


maxFlow' :: (Show i, Ix i) => Flow i Int -> i -> i -> Flow i Int
maxFlow' flow start end = maxFlow topo start end flow
    where
        ks = M.keys flow
        min' = minimum $ map (uncurry min) ks
        max' = maximum $ map (uncurry max) ks
        topo = buildTopo min' max' `addEdges` ks
