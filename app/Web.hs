module Main where

import System.Environment
import Lib

getPort :: [String] -> Int
getPort [] = 8000
getPort (x:_) = read x

main :: IO ()
main = do
      port <- getPort <$> getArgs                  -- IO [String]
      perHaskellWeb port

