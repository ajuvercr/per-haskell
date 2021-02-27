{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Json
    ( JsonPlanet(..)
    , JsonExpedition(..)
    , JsonState(..)
    , JsonMove(..)
    , Moves(..)
    , encodeMoves
    , decodeState
    , fromExpeditions
    )
where

import           Data.Aeson
import           Data.ByteString.Lazy.UTF8     as BLU -- from utf8-string


---- JsonPlanet ----

data JsonPlanet =
    JsonPlanet { name               :: String
           , x                  :: Float
           , y                  :: Float
           , owner              :: Int
           , power              :: Int
             } deriving (Show)

instance FromJSON JsonPlanet where
    parseJSON = withObject "planet" $ \o -> do
        name  <- o .: "name"
        x     <- o .: "x"
        y     <- o .: "y"
        owner <- o .:? "owner" .!= 0
        power <- o .: "ship_count"
        return JsonPlanet { .. }


---- JsonExpedition ----

data JsonExpedition =
    JsonExpedition  { id              :: Int
                    , origin          :: String
                    , destination     :: String
                    , ttl             :: Int
                    , sender          :: Int
                    , ship_count      :: Int
                    } deriving (Show)

instance FromJSON JsonExpedition where
    parseJSON = withObject "expedition" $ \o -> do
        id          <- o .: "id"
        origin      <- o .: "origin"
        destination <- o .: "destination"
        ttl         <- o .: "turns_remaining"
        sender      <- o .: "owner"
        ship_count  <- o .: "ship_count"
        return JsonExpedition { .. }


---- JsonState ----

data JsonState =
    JsonState { planets                 :: [JsonPlanet]
              , exps                    :: [JsonExpedition]
              } deriving (Show)

instance FromJSON JsonState where
    parseJSON = withObject "state" $ \o -> do
        planets <- o .: "planets"
        exps    <- o .: "expeditions"
        return JsonState { .. }


---- JsonMove ----

data JsonMove =
    JsonMove { from                     :: String
             , to                       :: String
             , force                    :: Int
             } deriving (Show)
instance ToJSON JsonMove where
    toJSON JsonMove {..} =
        object ["origin" .= from, "destination" .= to, "ship_count" .= force]

-- Make a move from an expedition
fromExpedition :: JsonExpedition -> JsonMove
fromExpedition (JsonExpedition _ from to _ _ force) = JsonMove { .. }


---- Moves ----

newtype Moves = Moves { moves :: [JsonMove] } deriving (Show)
instance ToJSON Moves where
    toJSON Moves {..} = object ["moves" .= moves]

encodeMoves :: Moves -> String
encodeMoves = BLU.toString . encode

decodeState :: String -> Either String JsonState
decodeState = eitherDecode . BLU.fromString

-- Expeditions to Moves only the ones with a negative id
fromExpeditions :: [JsonExpedition] -> Moves
fromExpeditions es = Moves { moves = map fromExpedition $ onlySmallExps es }

-- Filter expeditions on negative id
onlySmallExps :: [JsonExpedition] -> [JsonExpedition]
onlySmallExps = filter (\x -> Json.id x == (-1))
