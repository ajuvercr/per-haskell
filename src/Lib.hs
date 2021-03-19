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
    let suppliers = buildSuppliers state
        actionGraph = buildActionGraph state
    actions <- calculateActions (time + 1000) suppliers actionGraph -- This is shorter (here 0.1 seconds)
    return $ intoMoves actions


-- |Timeout -> State -> Actions
calculateActions :: Int -> Suppliers -> ActionGraph -> IO ActionGraph
calculateActions tt sups ag =
    -- do
    -- bestMove <- bestMove state
    -- return $ maybeToList $ traceShowId bestMove
    do
        time <- getMicros
        action <- timeout (tt - time) (return $ tryApply sups ag)
        case action of
            Nothing -> return ag
            Just (False, _, ag') -> return ag'
            -- Just (True, sups', ag') -> return ag'
            Just (True, sups', ag') -> calculateActions tt sups' ag'
