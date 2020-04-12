module Lib
    ( perHaskell
    )
where

import           Data.Aeson
import           Types
import Data.Either.Combinators
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy          as B
import           GHC.Generics

import System.IO (hPutStrLn, stderr)


perHaskell :: IO ()
perHaskell = do
    line <- getLine
    case handleLine line of
        Right x -> putStrLn x
        Left x -> hPutStrLn stderr x
    perHaskell

handleLine :: String -> Either String String
handleLine x = mapRight (encodeMoves . handleState) (decodeState x)

handleState :: State -> Moves
handleState x = Moves [ Move "Origin" "Dest" 2 ]
