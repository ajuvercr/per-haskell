module Lib
    ( perHaskell
    , commutative
    )
where

import           Data.Aeson
import           Json
import Types
import Util
import Data.Either.Combinators
import System.Timeout
import Control.Monad.Extra
import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Lazy          as B
import           GHC.Generics

import System.IO (hPutStrLn, stderr, hFlush, stdout)



perHaskell :: IO ()
perHaskell = do
    line <- getLine
    out <- handleLine line
    case out of
        Right x -> putStrLn x >> hFlush stdout
        Left x -> hPutStrLn stderr x
    perHaskell


handleLine :: String -> IO (Either String String)
handleLine x = mapM f (decodeState x)
    where f st = encodeMoves <$> handleState (transformInput st)


handleState :: [Planet] -> IO Moves
handleState state = do
    time <- getMicros
    actions <- calculateActions (time + 100000) state -- This is shorter (here 0.1 seconds)
    return $ Moves $ concatMap actionToMove actions


actionToMove :: Action -> [JsonMove]
actionToMove Action {participation=parts, target=target} = map f parts
    where f (p, force) = JsonMove (Types.name p) (Types.name target) force


-- |Timeout -> State -> Actions
calculateActions :: Int -> [Planet] -> IO [Action]
calculateActions tt state = do
    time <- getMicros
    action <- timeout (tt - time) (bestMove state)
    case action of
        Nothing -> return []
        Just Nothing -> return []
        Just (Just x) -> (x:) <$> calculateActions tt (applyAction state x)


applyAction :: [Planet] -> Action -> [Planet]
applyAction ps _ = ps

-- Failed, Succeeded with, Succeeded without (empty)
bestMove :: [Planet] -> IO (Maybe Action)
bestMove state = pure $ maxBy actionPower actions
        where actions = undefined :: [Action]

actionPower :: Action -> Float
actionPower _ = 1.0
