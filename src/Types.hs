module Types
    ( Coord
    , Alliance (..)
    , Planet (..)
    , Action (..)
    , distance
    , transformInput
    )
where

import Json
import Util

type Coord = (Float, Float)

data Alliance  = Hostile | Friendly | Neutral deriving (Show, Eq)

data Planet =
     Planet { name      :: String
            , loc       :: Coord
            , alliance  :: Alliance
            , power     :: [Int]
            } deriving (Show)


data Action = Action
            { offensive :: Bool
            , participation :: [(Planet, Int)]
            , target :: Planet
            } deriving (Show)

distance :: Planet -> Planet -> Int
distance p1 p2 =
    let dx = x1 - x2
        dy = y1 - y2
    in  ceiling . sqrt $ dx * dx + dy * dy
    where
        (x1, y1) = loc p1
        (x2, y2) = loc p2

getAlliance :: Int -> Alliance
getAlliance 0 = Neutral
getAlliance 1 = Friendly
getAlliance _ = Hostile



transformPlanet :: [JsonExpedition] -> JsonPlanet -> Planet
transformPlanet exps (JsonPlanet name x y owner power) = Planet name (x, y) alliance power
    where
        alliance = getAlliance owner
        base = if alliance == Neutral
               then repeat 0
               else [1..]
        important = filter (\e -> destination e == name) exps
        update base exp = if sender exp == owner
                          then applyAt (+ ship_count exp) (ttl exp) base
                          else applyAt (subtract $ ship_count exp) (ttl exp) base
        power = foldl update base important


transformInput :: JsonState -> [Planet]
transformInput (JsonState planets exps) = map (transformPlanet exps) planets
