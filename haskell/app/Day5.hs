{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Day5 where

import Control.Arrow ((&&&))
import Text.RawString.QQ
import Util

type Seed = Int

type Range = (Seed, Seed)

type Locations = [Range]

type Transform = (Int, Range)

type Almanac = ([Range], [[Transform]])

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

loadSeeds :: String -> [Range]
loadSeeds = fmap (\x -> (x, x + 1)) . parse
  where
    parse = fmap read . drop 1 . splitAtAll ' '

makeRange :: (Num b) => [b] -> [(b, b)]
makeRange [] = []
makeRange (x : y : rest) = (x, x + y) : makeRange rest

loadSeeds2 :: String -> [Range]
loadSeeds2 = makeRange . parse
  where
    parse = fmap read . drop 1 . splitAtAll ' '

loadTransforms :: [String] -> [Transform]
loadTransforms = fmap ((\[dest, src, range] -> (dest, (src, src + range))) . fmap read . splitAtAll ' ')

loadAlmanac :: (String -> [Range]) -> [String] -> Almanac
loadAlmanac loadSeeds = loadSeeds . head &&& fmap (loadTransforms . drop 1) . splitAtAll "" . tail

within :: Range -> Range -> Bool
within (start, end) (start', end') = start >= start' && end <= end'

splitRange :: Range -> Range -> [Range]
splitRange r1@(start, end) r2@(start', end')
  | within r1 r2 || start >= end' || end <= start' = [r1]
  | start < start' = (start, start') : splitRange (start', end) r2
  | end > end' = [(start, end'), (end', end)]

createPipe :: Transform -> (Range -> [(Bool, Range)])
createPipe (dest, transformRange@(src, end)) seedRange =
  fmap
    ( \range@(rstart, rend) ->
        if range `within` transformRange
          then (True, (rstart - src + dest, rend - src + dest))
          else (False, range)
    )
    (splitRange seedRange transformRange)

createPipes :: [Transform] -> (Range -> Locations)
createPipes [t] range = snd <$> createPipe t range
createPipes (t@(dest, tRange) : ts) range =
  (snd <$> filter fst transform) ++ (createPipes ts . snd =<< filter (not . fst) transform)
  where
    transform = createPipe t range
    destRange = (dest, dest + (snd tRange - fst tRange))

processSeed :: Range -> [[Transform]] -> [Range]
processSeed seed = foldl (\ranges trans -> createPipes trans =<< ranges) [seed]

loadLocations :: Almanac -> [Range]
loadLocations ([], _) = []
loadLocations (seeds, transforms) = flip processSeed transforms =<< seeds

day5_1 = show . minimum . fmap fst . loadLocations . loadAlmanac loadSeeds . lines

day5_2 = show . minimum . fmap fst . loadLocations . loadAlmanac loadSeeds2 . lines
