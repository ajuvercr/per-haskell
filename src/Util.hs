module Util
where

import           Control.Applicative
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Monoid
import qualified Data.Map as M

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

xBy :: (b -> b -> Bool) -> (a -> b) -> [a] -> Maybe a
xBy g f = foldl (\p a -> (\x -> if' (g (f x) (f a)) x a) <$> p <|> Just a) Nothing


minBy :: Ord b  => (a -> b) -> [a] -> Maybe a
minBy = xBy (<)


maxBy :: Ord b  => (a -> b) -> [a] -> Maybe a
maxBy = xBy (>)

mCons :: Maybe a -> [a] -> [a]
mCons ma xs = maybe xs (:xs) ma

getMicros :: IO Int
getMicros = round . (* 1000000) <$> getPOSIXTime

commutative :: Num a => [a] -> [a]
commutative [] = []
commutative [a] = [a]
commutative (x:y:xs) = x:commutative (x+y:xs)


applyAt :: (a -> a) -> Int -> [a] -> [a]
applyAt _ _ []     = []
applyAt f 0 (x:xs) = f x:xs
applyAt f i (x:xs) = x : applyAt f (i-1) xs


orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse _ x = x


applyAll :: (Ord i) => M.Map i a -> [(i, a -> a)] -> M.Map i a
applyAll map [] = map
applyAll map ((key, alter):xs) = M.adjust alter key $ applyAll map xs
