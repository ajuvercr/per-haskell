{-# LANGUAGE TemplateHaskell,TupleSections #-}

module Logic
    ( Suppliers
    , ActionGraph
    , buildActionGraph
    , buildSuppliers
    , intoMoves
    , tryApply
    ) where


import Types ( Planet, name, yours, distance, powerAt, notYours, Alliance (Neutral, Friendly), alliance)
import Util
import Data.Maybe
import Control.Lens hiding (element)
import Control.Lens.TH
-- import Data.List
import Data.SortedList ( SortedList )
import qualified Data.SortedList as SL
import Data.List.Extra
import Graph (Flow, GraphTopo, addEdge, addEdges, buildTopo, maxFlow)
import Data.Map (Map)
import qualified Data.Map as Map
import Json
import qualified Control.Monad.State.Lazy as St
import Debug.Trace
import Control.Applicative ((<|>))



type Suppliers = SortedList ActionSupplier
type TransformGraph = ActionGraph -> ActionGraph

data ActionSupplier = ActionSupplier
    { ap        :: Float
    , target    :: Planet
    , transform :: TransformGraph
    , next      :: Maybe ActionSupplier
    , offensive :: Bool
    }

instance Eq ActionSupplier where
    a1 == a2 = target a1 == target a2 && ap a1 == ap a2

instance Ord ActionSupplier where
    compare a1 a2 = ap a1 `compare` ap a2

instance Show ActionSupplier where
    show (ActionSupplier ap t _ _ True) = "Offensive Supplier at " ++ show ap ++ " targeting " ++ Types.name t
    show (ActionSupplier ap t _ _ False) = "Defensive Supplier at " ++ show ap ++ " targeting " ++ Types.name t

source :: Int
source = -1

dest :: Int
dest = -2

alot :: Int
alot = 1000


data ActionGraph = ActionGraph
    { _topo     :: GraphTopo Int
    , _flow     :: Flow Int Int
    , _ours     :: Map Planet Int
    , _targets  ::  Map Planet Int
    } deriving (Show)

$(makeLenses ''ActionGraph)

buildActionGraph :: [Planet] -> ActionGraph
buildActionGraph state = ActionGraph (buildTopo dest t_l `addEdges` ours_topos) (Map.fromList ours_flows) (Map.fromList oursf) (Map.fromList targetsf)
    where
        ys = yours state
        ys_l = length ys
        t_l = length state + ys_l
        (ours_flows, ours_topos, oursf) = unzip3 $ zipWith (\p i -> (((source, i), powerAt p 0 - 1), (source, i), (p, i))) ys [0..]
        targetsf = zip state [ys_l..]


sortOnDistance :: Planet -> [Planet] -> [Planet]
sortOnDistance planet = sortOn $ distance planet


addTarget :: Planet -> Int -> ActionGraph -> ActionGraph
addTarget target a graph = graph
    & flow %~ Map.insert (id, dest) p
    & topo %~ flip addEdge (id, dest)
    where
        Just id = view (targets . at target) graph
        p = powerAt target a + 1


buildActionSupplier :: [Planet] -> Planet -> Maybe ActionSupplier
buildActionSupplier state target = ba (addTarget target) $ sortOnDistance target $ filter (/= target) $ yours state
    where
        ba f (p:ps) = let d = distance target p
                          (ps', rest) = span (\x -> d == distance target x) (p:ps)
                          transform d x = foldr (<+> target) (f d x) ps' in
                (\x -> ActionSupplier x target (transform d) (ba transform rest) True) <$> calcAp p target d <|> ba transform rest
        ba _ [] = Nothing

threshold :: Float
threshold = 100.0

-- Minimal is better
calcAp :: Planet -> Planet -> Int -> Maybe Float
calcAp from target d
    | vat > 0 && o == Friendly = Nothing -- Don't send if it's friendly and stays friendly
    | vat < 0 && o /= Friendly = Nothing -- Don't send if it's hostile but will convert
    | o == Friendly            = Just $ 1.2 * d' -- Defense is useless
    | otherwise                = Just $ 0.8 * d' -- Offense is king
    where
        vat = powerAt target d
        tt = 0
        o = alliance target
        d' = fromIntegral d :: Float


buildSuppliers :: [Planet] -> Suppliers
buildSuppliers state = SL.toSortedList $ mapMaybe (buildActionSupplier state) state
    where
        ours = yours state



(<+>) :: Planet -> Planet -> ActionGraph -> ActionGraph
p1 <+> p2 = add'
    where
        add' graph = graph
            & flow %~ Map.insert (id1, id2) alot
            & topo %~ flip addEdge (id1, id2)
            where
                Just id1 = view (ours    . at p1) graph
                Just id2 = view (targets . at p2) graph


intoMoves :: ActionGraph -> Moves
intoMoves (ActionGraph topo flow ours targets) = Moves ms
    where
        invMap = Map.fromList $ map (\(p, i) -> (i, Types.name p)) (Map.assocs ours ++ Map.assocs targets)
        flow' = maxFlow topo source dest flow
        trans (i, j) = JsonMove (invMap Map.! i) (invMap Map.! j) <$> flow' Map.!? (i, j)
        ms = mapMaybe trans [(i, j) | i <- Map.elems ours, j <- Map.elems targets]


validGraph :: ActionGraph -> Bool
validGraph (ActionGraph topo flow _ ts) = all check targets
    where
        flow' = maxFlow topo source dest flow
        targets = Map.elems ts
        check i = (flow' Map.!? (i, dest)) == flow Map.!? (i, dest)


type State a = St.State Suppliers a

popSupplier :: Suppliers -> Maybe (Bool -> Suppliers, TransformGraph)
popSupplier s = case SL.uncons s of
        Just (a, s') -> do
            let f = Logic.transform a
                value = Logic.ap a
                a' = Logic.next a
            case a' of
                Just a'' -> Just (\x -> x ? SL.insert a'' s' :? s', f)
                Nothing  -> Just (const s', f)
        Nothing      -> Nothing


tryApply :: Suppliers -> ActionGraph -> (Bool, Suppliers, ActionGraph)
tryApply suppliers graph = case popSupplier suppliers of
        Just (s_f, f) -> let ag = f graph
                             valid = validGraph ag
                             ag' = valid ? ag :? graph
                       in (True, s_f valid, ag')
        Nothing -> (False, suppliers, graph)
