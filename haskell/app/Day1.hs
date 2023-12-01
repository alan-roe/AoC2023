module Day1 where

import Data.Char (digitToInt, isDigit)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Util

type CalibrationValue = Int

type CalibrationReader = (String -> CalibrationValue)

type Index = Int

type PossibleValue = (Index, Int)

numbers =
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

test =
  [ "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"
  ]

convert :: String -> Int
convert s =
  case readMaybe s of
    Just x -> x
    Nothing -> fromJust (s `elemIndex` numbers) + 1

findPossible :: String -> String -> [PossibleValue]
findPossible whole s@(_ : _) = findPossible' whole s 0
  where
    findPossible' :: String -> String -> Int -> [PossibleValue]
    findPossible' whole s offset =
      case indexOf s whole of
        Just i -> (i + offset, convert s) : findPossible' (drop (i + 1) whole) s (offset + i + 1)
        Nothing -> []

possibleValues :: String -> [PossibleValue]
possibleValues cs@(_ : _) = foldl1 (++) (fmap (findPossible cs) numbers)

retrieveValue2 :: CalibrationReader
retrieveValue2 s@(_ : _) = first * 10 + last
  where
    first = snd $ foldPossible (<)
    last = snd $ foldPossible (>)
    pvs = possibleValues s
    foldPossible f =
      foldl1
        ( \pv@(i, v) pv'@(i', v') ->
            if i `f` i'
              then pv
              else pv'
        )
        pvs

retrieveValue :: CalibrationReader
retrieveValue s = first * 10 + last
  where
    first = firstDigit s
    last = firstDigit $ reverse s
    firstDigit (c : cs) =
      if isDigit c
        then digitToInt c
        else firstDigit cs

day1_1 = show . sum . fmap retrieveValue . lines

day1_2 = show . sum . fmap retrieveValue2 . lines
