{-# LANGUAGE DeriveGeneric #-}

module Expedition
    ( Expedition (..)
    ) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Expedition =
    Expedition { id              :: Int
               , origin          :: !T.Text
               , destination     :: !T.Text
               , turns_remaining :: Int
               , owner           :: Int
               , ship_count      :: Int
               } deriving (Show, Generic)

instance FromJSON Expedition
instance ToJSON Expedition
