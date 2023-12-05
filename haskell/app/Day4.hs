module Day4 where

import Control.Arrow ((&&&), (***))
import Util

type WinningNumbers = [Int]

type CardNumbers = [Int]

type Card = (WinningNumbers, CardNumbers)

test =
  [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  ]

pair :: [b] -> (b, b)
pair [a, b] = (a, b)

loadCard :: [Char] -> Card
loadCard = (fmap read *** fmap read) . pair . splitAtAll "|" . drop 2 . splitAtAll ' '

winningNumbers :: Card -> CardNumbers
winningNumbers (win, card) = filter (`elem` win) card

doubleEach :: Int -> Int
doubleEach 0 = 0
doubleEach n = 2 ^ (n-1) 

day4_1 = show . sum . fmap (doubleEach . length . winningNumbers . loadCard) . lines
day4_2 s = show 0