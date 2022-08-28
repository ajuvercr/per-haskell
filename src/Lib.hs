{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( perHaskell
    , commutative
    , perHaskellWeb
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

import Data.Binary.Builder (putStringUtf8)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200, status404, ResponseHeaders, hContentType)

import Data.ByteString.Lazy(unpack)
import Data.Text.Internal.Unsafe.Char(unsafeChr8)

perHaskell :: IO ()
perHaskell = do
    line <- getLine
    out <- handleLine line
    case out of
        Right x -> putStrLn x >> hFlush stdout
        Left x -> hPutStrLn stderr x
    perHaskell

perHaskellWeb :: Int -> IO ()
perHaskellWeb port = do
    putStrLn $ "Listening on port " ++ show port
    run port app

headers :: ResponseHeaders
headers = [ (hContentType,  "application/json") ]

app req respond = do
    line <- map unsafeChr8 . unpack <$> strictRequestBody req
    out <- handleLine line
    respond $ case out of
        Right x -> responseBuilder status200 headers $ putStringUtf8 x
        Left x -> responseBuilder status404 headers $ putStringUtf8 x


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
    do
        time <- getMicros
        action <- timeout (tt - time) (return $ tryApply sups ag)
        case action of
            Nothing -> return ag
            Just (False, _, ag') -> return ag'
            -- Just (True, sups', ag') -> return ag'
            Just (True, sups', ag') -> calculateActions tt sups' ag'

