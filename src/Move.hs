{-# LANGUAGE DeriveGeneric #-}

module Move
    ( Move ( .. )
    ) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Move =
    Move { origin       :: !T.Text
         , destination  :: !T.Text
         , ship_count   :: Int
         } deriving (Show, Generic)

instance FromJSON Move
instance ToJSON Move
