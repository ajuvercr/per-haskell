module Lib
    ( perHaskell
    )
where

import           Data.Aeson
import           Types
import           Planet
import Data.Either.Combinators
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy          as B
import           GHC.Generics

import System.IO (hPutStrLn, stderr, hFlush, stdout)


perHaskell :: IO ()
perHaskell = do
    line <- getLine
    case handleLine line of
        Right x -> putStrLn x >> hFlush stdout
        Left x -> hPutStrLn stderr x
    perHaskell

handleLine :: String -> Either String String
handleLine x = mapRight (encodeMoves . handleState) (decodeState x)

handleState :: State -> Moves
handleState = doNotSoSimple
-- handleState x = case doSimple x of
--     Nothing -> Moves []
--     Just a -> Moves [a]
