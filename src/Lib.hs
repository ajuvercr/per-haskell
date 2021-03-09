{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}

module Lib
    ( perHaskell
    , commutative
    )
where

import Data.Aeson
import Json
import Types
import Util
import Logic
import System.Timeout

import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Debug.Trace
import Data.Maybe

import Control.Lens
import Data.Generics.Product
import Data.Generics.Sum

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
    -- print state
    time <- getMicros
    actions <- calculateActions (time + 100000) state -- This is shorter (here 0.1 seconds)
    return $ Moves $ concatMap actionToMove actions


actionToMove :: Action -> [JsonMove]
actionToMove Action {participation=parts, target=target} = map f parts
    where f (p, force) = JsonMove (Types.name p) (Types.name target) force


-- |Timeout -> State -> Actions
calculateActions :: Int -> [Planet] -> IO [Action]
calculateActions tt state =
    -- do
    -- bestMove <- bestMove state
    -- return $ maybeToList $ traceShowId bestMove
    do
    time <- getMicros
    traceShowM state
    action <- timeout (tt - time) (bestMove state)
    case action of
        Nothing -> return []
        Just Nothing -> return []
        Just (Just x) -> (x:) <$> calculateActions tt (applyAction state $ traceShowId x)


applyAction :: [Planet] -> Action -> [Planet]
applyAction ps Action{offensive=off, participation=parts, target=p} = p'
    where
        -- ps' = filter (/= p) ps
        foldF = if off
                then addExpedition subtract
                else addExpedition (+)
        mapF (from, amount) = (from, p, amount)
        p' = foldl foldF ps $ map mapF parts

-- TODO eigenlijk wil je het omgekeerd doen, maar ja
addExpedition :: (Int -> Int -> Int) -> [Planet] -> (Planet, Planet, Int) -> [Planet]
addExpedition addF state (from, to, a) = state
    & traverse . filtered (==to)   . field @"power" . traverseFrom ttl %~ addF a
    & traverse . filtered (==from) . field @"power" . traverseFrom 0   %~ subtract a
    where
        ttl = distance to from
        traverseFrom at = traversed . ifiltered (\i _ -> i >= at)

-- Failed, Succeeded with, Succeeded without (empty)
bestMove :: [Planet] -> IO (Maybe Action)
bestMove state = pure  $ maxBy actionPower $ allActions state
