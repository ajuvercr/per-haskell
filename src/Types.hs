
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Types
    ( Coord
    , Alliance (..)
    , Planet (..)
    , Action (..)
    , distance
    , transformInput
    , yours
    , notYours
    , powerAt
    ) where

import Json
import Util

import qualified Control.Monad.State.Lazy as State

import Control.Lens
import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics (Generic)
import Control.Lens.Combinators

import Data.Map (Map)



type Coord = (Float, Float)

data Alliance  = Hostile | Friendly | Neutral deriving (Show, Eq, Ord)

data Planet =
     Planet { name      :: String
            , loc       :: Coord
            , alliance  :: Alliance
            , power     :: [Int]
            } deriving (Generic)

instance Show Planet where
--   show Planet{Types.name=n, loc=l, alliance=a, Types.power=p} = "{ \"name\": " ++ show n ++ ", \"loc\": " ++ show l ++ ", \"alliance\": " ++ show a ++ ", \"power\": " ++ show pp
    -- where pp = take 5 p

    show Planet{Types.name=n, loc=l, alliance=a, Types.power=p} = show n ++ "@" ++ show l ++ " " ++ show a ++ " " ++ show (take 10 p)

instance Eq Planet where
    p1 == p2 = Types.name p1 == Types.name p2


-- Ordering on distance to center
instance Ord Planet where
  Planet {loc=loc1} `compare` Planet {loc=loc2} = loc1 `compare` loc2


data Action = Action
            { offensive :: Bool
            , participation :: [(Planet, Int)]
            , target :: Planet
            } deriving ()



instance Show Action where
  show Action{offensive=off, participation=parts, target=t} = "{\"target\": " ++ Types.name t ++ ", \"offensive\": " ++ show off ++ ", \"parts\": " ++ show ts ++ "}"
    where
        ts = map (over _1 Types.name) parts :: [(String, Int)]

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
transformPlanet exps (JsonPlanet name x y owner power) = Planet name (x, y) alliance p
    where
        alliance = getAlliance owner
        base = if alliance == Neutral
               then repeat power
               else [power..]
        important = filter (\e -> destination e == name) exps
        update base exp = if sender exp == owner
                          then applyAt (+ ship_count exp) (ttl exp) base
                          else applyAt (subtract $ ship_count exp) (ttl exp) base
        p = foldl update base important


transformInput :: JsonState -> [Planet]
transformInput (JsonState planets exps) = map (transformPlanet exps) planets

yours :: [Planet] -> [Planet]
yours = filter (\x -> alliance x == Friendly)

notYours :: [Planet] -> [Planet]
notYours = filter (\x -> alliance x /= Friendly)


powerAt :: Planet -> Int -> Int
powerAt Planet {Types.power=p} at = p!!at


-- |(Id, Options)
type Range = (Planet, [Int])

type MoveBuilder = Int -> JsonMove

-- |(Total, Participants)
type SumConstraint = (Int, [(Range, MoveBuilder)])

-- |Planet -> Range
type ConstraintsState = Map Planet Range
type St a = State.State ConstraintsState a

-- This is way too concrete
applySumConstraint :: SumConstraint -> St (Either () [JsonMove])
applySumConstraint (a, _) | a <= 0 = pure $ Right []
applySumConstraint (_, []) = pure $ Left ()
applySumConstraint (a, ((p, options), builder):xs) = do
    let forThis = max a $ maximum options
    let sumC = (a - forThis, xs)
    rest <- applySumConstraint sumC
    case rest of
        Left () -> return $ Left ()
        Right moves -> return $ Right $ builder forThis:moves
