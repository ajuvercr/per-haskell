{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
    ( perHaskell
    )
where

import           Data.Aeson
import qualified Planet
import qualified Expedition                    as Exp
import qualified Move
import qualified Data.Text                     as T
import Data.Either.Combinators
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy          as B
import           Data.ByteString.Lazy.UTF8     as BLU -- from utf8-string
import           GHC.Generics


data State =
    State { planets     :: [Planet.Planet]
          , expeditions :: [Exp.Expedition]
          } deriving (Show, Generic)

instance FromJSON State
instance ToJSON State

newtype Moves = Moves { moves     :: [Move.Move]
          } deriving (Show, Generic)

instance FromJSON Moves
instance ToJSON Moves

perHaskell :: IO ()
perHaskell = do
    line <- getLine
    putStrLn $ handleLine line
    perHaskell

handleLine :: String -> String
handleLine x = unpackEither $ mapRight (BLU.toString . encode . handleState) (decodeState $ BLU.fromString x)

decodeState :: ByteString -> Either String State
decodeState = eitherDecode

handleState :: State -> Moves
handleState x = Moves [ Move.Move "Origin" "Dest" 2 ]

unpackEither :: Either a a -> a
unpackEither x = case x of
    Left x -> x
    Right x -> x
