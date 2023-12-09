{-# LANGUAGE QuasiQuotes #-}

module Day5 where

import Control.Monad (join)
import Data.List (sortBy)
import Text.RawString.QQ
import Util

type Seed = Int

type Location = Int

data Almanac = Almanac [Seed] (Seed -> Location)

test =
  [r|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4|]

seeds :: Almanac -> [Seed]
seeds (Almanac seeds _) = seeds

locations :: Almanac -> [Location]
locations (Almanac a f) = fmap f a

loadAlmanac :: [String] -> Almanac
loadAlmanac alm = Almanac ((fmap read . drop 1 . splitAtAll ' ' . head) (head spl)) (loadLocations (tail spl))
  where
    spl = splitAtAll "" alm

createPipe :: [Int] -> (Seed -> Location)
createPipe [dest, src, range] seed =
  if seed >= src && seed < src + range
    then seed - src + dest
    else seed
createPipe xs s = error $ show xs

createPipes :: [[Int]] -> (Seed -> Location)
createPipes [s] seed = createPipe s seed
createPipes (s : ss) seed =
  if transform == seed
    then createPipes ss seed
    else transform
  where
    transform = createPipe s seed

sortByIndex :: Int -> [[Int]] -> [[Int]]
sortByIndex i = sortBy (flip (\x y -> compare (head x) (head y)))

loadLocations :: [[String]] -> (Seed -> Location)
loadLocations s = foldr1 (.) $ reverse (fmap (createPipes . fmap (fmap read . splitAtAll ' ') . drop 1) s)

day5_1 = show . minList . locations . loadAlmanac . lines

day5_2 s = show 0
