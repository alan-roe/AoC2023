{-# LANGUAGE InstanceSigs #-}

module Day7 where

import Control.Arrow ((&&&), (***))
import Data.Char (digitToInt)
import Data.List (group, sort, sortBy)
import Util (pair, splitAtAll)

data HandType
  = High
  | OneP
  | TwoP
  | ThreeK
  | Full
  | FourK
  | FiveK
  deriving (Eq, Ord, Show)

data Card
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | T
  | J
  | Q
  | K
  | A
  deriving (Eq, Ord, Show, Enum)

type Bid = Int

type Hand = ([Card], HandType)

type Play = (Hand, Bid)

type CardReader = Char -> Card

type HandReader = String -> Hand

test =
  [ "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  ]

readCard :: Card -> CardReader
readCard jCard c =
  case c of
    'T' -> T
    'J' -> jCard
    'Q' -> Q
    'K' -> K
    'A' -> A
    _ -> toEnum $ digitToInt c - 1

loadHand :: CardReader -> String -> Hand
loadHand cr hand@(c : cs)
  | all (== c) cs = (cards, FiveK)
  | otherwise =
      (,)
        cards
        ( case length grouped of
            5 -> if [Joker] `elem` grouped then OneP else High
            4 -> if [Joker] `elem` grouped || [Joker, Joker] `elem` grouped then ThreeK else OneP
            3 ->
              if any ((== 3) . length) grouped
                then if replicate 3 Joker `elem` grouped || [Joker] `elem` grouped then FourK else ThreeK
                else if [Joker] `elem` grouped then Full else if [Joker, Joker] `elem` grouped then FourK else TwoP
            2 ->
              if any ((== 4) . length) grouped
                then if [Joker] `elem` grouped || replicate 4 Joker `elem` grouped then FiveK else FourK
                else if replicate 2 Joker `elem` grouped || replicate 3 Joker `elem` grouped then FiveK else Full
        )
  where
    grouped = (group . sort) cards
    cards = fmap cr hand

playSort :: Play -> Play -> Ordering
playSort ((cards, handType), _) ((cards', handType'), _) =
  if handType == handType'
    then compare cards cards'
    else compare handType handType'

loadPlay :: HandReader -> String -> Play
loadPlay f = (f *** read) . pair . words

day7_1 = show . sum . fmap (uncurry (*)) . zip [1 ..] . fmap snd . sortBy playSort . fmap (loadPlay (loadHand (readCard J))) . lines

day7_2 = show . sum . fmap (uncurry (*)) . zip [1 ..] . fmap snd . sortBy playSort . fmap (loadPlay (loadHand (readCard Joker))) . lines
