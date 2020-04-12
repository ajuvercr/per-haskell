{-# LANGUAGE DeriveGeneric #-}

module Planet
    ( Planet (..)
    ) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Planet =
    Planet { name               :: !T.Text
           , x                  :: Float
           , y                  :: Float
           , owner              :: Maybe Int
           , ship_count         :: Int
             } deriving (Show, Generic)

instance FromJSON Planet
instance ToJSON Planet
