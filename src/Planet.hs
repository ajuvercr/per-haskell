module Planet
    ( planetPower
    , distance
    , doSimple
    )
where

import           Types
import           Control.Applicative

type Exp = (Int, Int)


-- Calcululate the amount of ships to defeat in turns
planetPower :: Int -> State -> Planet -> Int
planetPower n (State _ exps) p =
    power p
        + sum (map ship_count $ filter (filterExpedition n p (== owner p)) exps)
        - sum (map ship_count $ filter (filterExpedition n p (/= owner p)) exps)


-- Filter for Expedition, filtering ttl, destination and extra sender bound
filterExpedition :: Int -> Planet -> (Int -> Bool) -> Expedition -> Bool
filterExpedition n p c x =
    ttl x <= n && c (sender x) && destination x == name p


getFromName :: State -> String -> Planet
getFromName (State ps _) n = firstWith (\x -> name x == n) ps

-- Filter all neutral planets
neutral :: [Planet] -> [Planet]
neutral = filter (\x -> 0 == owner x)


-- Filter all your planets
your :: [Planet] -> [Planet]
your = filter (\x -> 1 == owner x)

notYour :: [Planet] -> [Planet]
notYour = filter (\x -> 1 /= owner x)

-- Filter all enemy planets
enemy :: [Planet] -> [Planet]
enemy = filter (\x -> 1 < owner x)


-- Calculate distance between planets
distance :: Planet -> Planet -> Int
distance p1 p2 =
    let dx = x p1 - x p2
        dy = y p1 - y p2
    in  ceiling . sqrt $ dx * dx + dy * dy


--
doSimple :: State -> Maybe Move
doSimple (State ps exps) = do
    source <- maxBy power $ your ps
    target <- minBy power $ notYour ps
    return $ Move (name source) (name target) (power target + 1)

tryHead :: [a] -> Maybe a
tryHead (x:xs) = Just x
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
firstWith f as = head $ filter f as

mPush :: Maybe a -> [a] -> [a]
mPush Nothing x = x
mPush (Just a) xs = a:xs

smallExp :: State -> String -> String -> Int -> Expedition
smallExp state origin destination
    = Expedition 0 origin destination
        (distance (getFromName state origin)
            (getFromName state destination))
        0
