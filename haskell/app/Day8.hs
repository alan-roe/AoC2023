module Day8 where

import Control.Monad (join)
import Data.Map (Map)
import Util (lookup')

type Instructions = [Char]

type Label = String

type Node = (Label, (Label, Label))

type Network = (Instructions, [Node])

test =
  [ "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  ]

test2 =
  [ "LR",
    "",
    "11A = (11B, XXX)",
    "11B = (XXX, 11Z)",
    "11Z = (11B, XXX)",
    "22A = (22B, XXX)",
    "22B = (22C, 22C)",
    "22C = (22Z, 22Z)",
    "22Z = (22B, 22B)",
    "XXX = (XXX, XXX)"
  ]

loadNodes :: [[String]] -> [Node]
loadNodes [] = []
loadNodes ([label, _, left, right] : rest) = (label, ((take 3 . drop 1) left, take 3 right)) : loadNodes rest

loadNetwork :: [String] -> Network
loadNetwork (instructions : _ : nodes) = (instructions, loadNodes (fmap words nodes))

aToZ' :: Network -> Label -> Int -> Int
aToZ' (instruction : ins, nodes) label count = if nextLabel == "ZZZ" then count + 1 else aToZ' (ins, nodes) nextLabel count + 1
  where
    nextLabel = (if instruction == 'L' then fst else snd) (lookup' label nodes)

aToZ :: Network -> Int
aToZ (instructions, nodes) = aToZ' ((join . repeat) instructions, nodes) "AAA" 0

day8_1 = show . aToZ . loadNetwork . lines

day8_2 s = show 0
