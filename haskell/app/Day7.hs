module Day7 where

import Control.Arrow ((&&&), (***))
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

type Hand = (String, HandType)

data Card
  = Two
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
  deriving (Eq, Ord, Show)

type Bid = Int

type Play = (Hand, Bid)

test =
  [ "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  ]

-- Five of a kind, where all five cards have the same label: AAAAA
-- Four of a kind, where four cards have the same label and one card has a different label: AA8AA
-- Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
-- Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
-- Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
-- One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
-- High card, where all cards' labels are distinct: 23456
loadHand :: String -> Hand
loadHand hand@(c : cs) 
  | all (== c) cs = (hand, FiveK)


loadPlay :: String -> Play
loadPlay = (loadHand *** read) . pair . words

day7_1 s = show 0

day7_2 s = show 0