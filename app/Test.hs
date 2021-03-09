module Main where

import Lib

main :: IO ()
main = print (commutative [1..] !! 5)
