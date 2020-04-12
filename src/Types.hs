{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Types
    ( Planet(..)
    , Expedition(..)
    , State(..)
    , Move(..)
    , Moves(..)
    , encodeMoves
    , decodeState
    )
where

import           Data.Aeson
import           Data.ByteString.Lazy.UTF8     as BLU -- from utf8-string


---- Planet ----

data Planet =
    Planet { name               :: String
           , x                  :: Float
           , y                  :: Float
           , owner              :: Int
           , power              :: Int
             } deriving (Show)

instance FromJSON Planet where
    parseJSON = withObject "planet" $ \o -> do
        name  <- o .: "name"
        x     <- o .: "x"
        y     <- o .: "y"
        owner <- o .:? "owner" .!= 0
        power <- o .: "ship_count"
        return Planet { .. }


---- Expedition ----

data Expedition =
    Expedition { id              :: Int
               , origin          :: String
               , destination     :: String
               , ttl             :: Int
               , sender          :: Int
               , ship_count      :: Int
               } deriving (Show)

instance FromJSON Expedition where
    parseJSON = withObject "expedition" $ \o -> do
        id          <- o .: "id"
        origin      <- o .: "origin"
        destination <- o .: "destination"
        ttl         <- o .: "turns_remaining"
        sender      <- o .: "owner"
        ship_count  <- o .: "ship_count"
        return Expedition { .. }


---- State ----

data State =
    State { planets                 :: [Planet]
          , epxs                    :: [Expedition]
          } deriving (Show)

instance FromJSON State where
    parseJSON = withObject "state" $ \o -> do
        planets <- o .: "planets"
        epxs    <- o .: "expeditions"
        return State { .. }


---- Move ----

data Move =
    Move { from                     :: String
         , to                       :: String
         , force                    :: Int
         } deriving (Show)
instance ToJSON Move where
    toJSON Move {..} =
        object ["origin" .= from, "destination" .= to, "ship_count" .= force]


---- Moves ----

newtype Moves = Moves { moves :: [Move] } deriving (Show)
instance ToJSON Moves where
    toJSON Moves {..} = object ["moves" .= moves]


encodeMoves :: Moves -> String
encodeMoves = BLU.toString . encode

decodeState :: String -> Either String State
decodeState = eitherDecode . BLU.fromString
