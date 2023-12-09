module Day6 where

import Control.Monad (join)
import Util (pair, splitAtAll)

type Time = Int

type Distance = Int

type Race = (Time, Distance)

type Speed = Int

test =
  [ "Time:      7  15   30",
    "Distance:  9  40  200"
  ]

beatRecord :: Race -> [Int]
beatRecord (record, distance) = beatRecord' (record - 1)
  where
    beatRecord' 0 = []
    beatRecord' time =
      if newDistance > distance
        then newDistance : beatRecord' (time - 1)
        else beatRecord' (time - 1)
      where
        newDistance = (record - time) * time

loadRaces :: [String] -> [Race]
loadRaces = uncurry zip . pair . fmap (fmap read . drop 1 . splitAtAll ' ')

loadRaces2 :: [String] -> Race
loadRaces2 = pair . fmap (read . join . drop 1 . splitAtAll ' ')

day6_1 = show . product . fmap (length . beatRecord) . loadRaces . lines

day6_2 = show . length . beatRecord . loadRaces2 . lines