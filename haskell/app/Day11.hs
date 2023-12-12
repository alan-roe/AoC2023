{-# LANGUAGE QuasiQuotes #-}

module Day11 where

import Data.List (findIndices)
import Text.RawString.QQ
import Util (maxList, uniquePairs)

type Universe = [(Int, Int)]

test =
  [r|...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....|]

loadUniverse :: [String] -> Universe
loadUniverse =
  foldl (\l (y, objs) -> l ++ fmap (\(x, obj) -> (x, y)) (filter (\(_, obj) -> obj == '#') objs)) []
    . zip [0 ..]
    . fmap (zip [0 ..])

emptySpaces :: Universe -> ([Int], [Int])
emptySpaces u = (filter (\x -> not $ any (\(x', y') -> x' == x) u) [0 .. maxX], filter (\y -> not $ any (\(x', y') -> y' == y) u) [0 .. maxY])
  where
    maxX = maximum $ fmap fst u
    maxY = minimum $ fmap snd u

expandUniverse :: Int -> Universe -> Universe
expandUniverse amount u =
  fmap (\(x, y) -> (x + (amount - 1) * length (findIndices (x >) xs), y + (amount - 1) * length (findIndices (y >) ys))) u
  where
    emptySpace = emptySpaces u
    xs = fst emptySpace
    ys = snd emptySpace

objectDistance :: (Num a) => (a, a) -> (a, a) -> a
objectDistance (x, y) (x', y') = abs (x' - x) + abs (y' - y)

objectDistances :: Universe -> [Int]
objectDistances u = fmap (uncurry objectDistance) (uniquePairs u)

day11_1 = show . sum . objectDistances . expandUniverse 1 . loadUniverse . lines

day11_2 = show . sum . objectDistances . expandUniverse 1000000 . loadUniverse . lines
