{-# LANGUAGE TupleSections #-}
module Util where

import Data.List (elemIndex, findIndex, groupBy, isInfixOf, isPrefixOf, tails)
import Data.Maybe (fromJust)

-- Splits a list into a list of lists at each point that a occurs, discards the a token
splitAtAll :: (Eq a) => a -> [a] -> [[a]]
splitAtAll x xs = filter (\y -> head y /= x) $ groupBy (\a b -> a /= x && b /= x) xs

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a1 a2 = fmap (\x -> if x == a1 then a2 else x)

elemIndex' :: (Eq a, Show a) => a -> [a] -> Int
elemIndex' x xs = case elemIndex x xs of
  Just x -> x
  Nothing -> error ("no index" ++ show x ++ "in " ++ show xs)

swap :: (b, a) -> (a, b)
swap (x, y) = (y, x)

contains :: (Eq a) => [a] -> [a] -> Bool
contains = isInfixOf

mcons :: Maybe a -> [a] -> [a]
mcons (Just x) ys = x : ys
mcons Nothing ys = ys

indexOf :: (Eq a) => [a] -> [a] -> Maybe Int
indexOf sub str = findIndex (isPrefixOf sub) (tails str)

pair :: [a] -> (a, a)
pair [x, y] = (x, y)

lookup' :: Eq a => a -> [(a, b)] -> b
lookup' a = fromJust . lookup a

uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs xs = map (head xs,) (tail xs) ++ uniquePairs (tail xs)
