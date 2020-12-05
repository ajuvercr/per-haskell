module Planet
    ( planetPower
    , distance
    , doSimple
    , doNotSoSimple
    , doLessSimple
    )
where

import           Debug.Trace
import           Types
import           Control.Applicative
import Data.Maybe

type Exp = (Int, Int)


-- |Calcululate the amount of ships to defeat in turns
planetPower :: Int -> State -> Planet -> Int
planetPower n (State _ exps) p =
    power p
        + sum (map ship_count $ filter (filterExpedition n p (== owner p)) exps)
        - sum (map ship_count $ filter (filterExpedition n p (/= owner p)) exps)


-- |Filter for Expedition, filtering ttl, destination and extra sender bound
filterExpedition :: Int -> Planet -> (Int -> Bool) -> Expedition -> Bool
filterExpedition n p c x =
    ttl x <= n && c (sender x) && destination x == name p


getFromName :: State -> String -> Planet
getFromName (State ps _) n = firstWith (\x -> name x == n) ps


-- |Filter all neutral planets
neutral :: [Planet] -> [Planet]
neutral = filter (\x -> 0 == owner x)


-- Filter all your planets
yours :: State -> [Planet]
yours (State planets _) = your planets

-- |Filter all your planets
your :: [Planet] -> [Planet]
your = filter (\x -> 1 == owner x)

notYours :: State -> [Planet]
notYours (State planets _) = notYour planets

notYour :: [Planet] -> [Planet]
notYour = filter (\x -> 1 /= owner x)


-- |Filter all enemy planets
enemy :: [Planet] -> [Planet]
enemy = filter (\x -> 1 < owner x)


-- |Calculate distance between planets
distance :: Planet -> Planet -> Int
distance p1 p2 =
    let dx = x p1 - x p2
        dy = y p1 - y p2
    in  ceiling . sqrt $ dx * dx + dy * dy


-- TODO this could use some fine tuning, 1 or -1 is somewhat extreme
incoming :: Planet -> Expedition -> Int
incoming p exp
    | destination exp == name p = if' (owner p == sender exp) (-1) 1
    | otherwise = 0


incomings :: Planet -> [Expedition] -> Int
incomings p = sum . map (incoming p)



targetsfilter :: Planet -> (Planet -> Bool)
targetsfilter p = f
    where f e = owner p /= owner e && (power p + 1) > power e


shortesttarget :: [Planet] -> Planet -> Maybe Planet
shortesttarget ps p = minBy (distance p) $ filter (targetsfilter p) ps


shortesttargetmove :: [Planet] -> Planet -> Maybe Move
shortesttargetmove ps source = do
    target <- shortesttarget ps source
    return $ Move (name source) (name target) (power target + 1)


--
doSimple :: State -> Maybe Move
doSimple (State ps _) = do
    source <- maxBy power $ your ps
    target <- minBy power $ notYour ps
    return $ Move (name source) (name target) (power target + 1)


doLessSimple :: State -> [Move]
doLessSimple (State ps _) = mapMaybe (shortesttargetmove ps) (your ps)


tryHead :: [a] -> Maybe a
tryHead (x:_) = Just x
tryHead _ = Nothing


xBy :: (b -> b -> Bool) -> (a -> b) -> [a] -> Maybe a
xBy g f = foldl (\p a -> (\x -> if' (g (f x) (f a)) x a) <$> p <|> Just a) Nothing


minBy :: Ord b  => (a -> b) -> [a] -> Maybe a
minBy = xBy (<)


maxBy :: Ord b  => (a -> b) -> [a] -> Maybe a
maxBy = xBy (>)


if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


firstWith :: (a -> Bool) -> [a] -> a
firstWith f = head . filter f


mPush :: Maybe a -> [a] -> [a]
mPush Nothing x = x
mPush (Just a) xs = a:xs


smallExp :: State -> String -> String -> Int -> Expedition
smallExp state origin destination
    = Expedition (-1) origin destination
        (distance (getFromName state origin)
            (getFromName state destination))
        1   -- this is sender, not ship_count

-- Do tryPlanet over all your planets, folding on state
doNotSoSimple :: State -> Moves
doNotSoSimple state = fromExpeditions $ Types.epxs $ foldl tryPlanet state $ yours state

-- Main reduce function
tryPlanet :: State -> Planet -> State
tryPlanet s ps = traceShow ps s
