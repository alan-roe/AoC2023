{-# LANGUAGE InstanceSigs #-}

module Day2 where

import Data.Map (Map, findWithDefault, fromList, toList, union, (!))
import Util (splitAtAll)

data Cube
  = Red
  | Green
  | Blue
  deriving (Show, Ord, Eq)

type GameID = Int

data Game = Game GameID [Round]
  deriving (Show)

type Round = Map Cube Int

type Bag = Map Cube Int

instance Read Cube where
  readsPrec :: Int -> ReadS Cube
  readsPrec _ = parse
    where
      parse ('r' : 'e' : 'd' : rest) = [(Red, rest)]
      parse ('g' : 'r' : 'e' : 'e' : 'n' : rest) = [(Green, rest)]
      parse ('b' : 'l' : 'u' : 'e' : rest) = [(Blue, rest)]

test =
  [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

loadRound number cube = fromList [(cube, number)]

loadRounds :: [String] -> [Round]
loadRounds [] = []
loadRounds (number : cube : rest) =
  case reads cube of
    [(cube, ",")] -> union (loadRound (read number) cube) (head next) : tail next
      where
        next = loadRounds rest
    [(cube, ";")] -> loadRound (read number) cube : loadRounds rest
    [(cube, "")] -> [loadRound (read number) cube]

loadGame :: String -> Game
loadGame s = loadGame' $ splitAtAll ' ' s
  where
    loadGame' :: [String] -> Game
    loadGame' ("Game" : id : rest) = Game (read $ take (length id - 1) id) $ loadRounds rest

validRound :: Bag -> Round -> Bool
validRound b r = all (\(cube, amount) -> amount <= (b ! cube)) (toList r)

validGame :: Bag -> Game -> Bool
validGame b (Game _ r) = all (validRound b) r

minimumCubes :: Game -> Bag
minimumCubes (Game _ rounds) = foldl compareBags defaultBag rounds
  where
    defaultBag = fromList [(Red, 0), (Green, 0), (Blue, 0)]
    compareBags :: Bag -> Bag -> Bag
    compareBags b1 b2 = fromList [(Red, getMax Red b1 b2), (Green, getMax Green b1 b2), (Blue, getMax Blue b1 b2)]
    getMax :: Cube -> Bag -> Bag -> Int
    getMax cube b1 b2 = if b1 ! cube >= findWithDefault 0 cube b2 then b1 ! cube else b2 ! cube

bagPower :: Bag -> Int
bagPower b = findWithDefault 1 Red b * findWithDefault 1 Green b * findWithDefault 1 Blue b

day2_1 = show . sum . fmap (validate . loadGame) . lines
  where
    validate game@(Game id _) = if validGame bag game then id else 0
    bag = fromList [(Red, 12), (Green, 13), (Blue, 14)]

day2_2 = show . sum . fmap (bagPower . minimumCubes . loadGame) . lines
