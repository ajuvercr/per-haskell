module Logic
    ( allActions
    , actionPower
    ) where


import Types
import Util
import Data.Maybe
import Data.List
import Data.List.Extra


sortOnDistance :: Planet -> [Planet] -> [Planet]
sortOnDistance planet = sortOn $ distance planet


offensiveActions :: [Planet] -> Planet -> Maybe Action
offensiveActions state target = closest
    where
        closets_tuples = sort $ map (\x -> (distance target x, x)) (yours state)
        closest = firstJust f closets_tuples
        f (d, planet) = if powerAt target d > 0 && powerAt target d < powerAt planet 0 - 1
                        then Just $ Action True [(planet, 1 + powerAt target d)] target
                        else Nothing


defensiveActions :: [Planet] -> Planet -> Maybe Action
defensiveActions state p = Nothing


allActions :: [Planet] -> [Action]
allActions state = mapMaybe (offensiveActions state) (notYours state) ++ mapMaybe (defensiveActions state) (yours state)


actionPower :: Action -> Float
actionPower Action { participation =par, target=target } = - fromIntegral val
    where
        val = maximum $ map (\(p, _) -> distance target p) par
