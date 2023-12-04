module Day3 where

import Data.Char (isDigit)
import Data.Map (toList)
import Data.Maybe (mapMaybe)
import Grid
import Util (replace)

type Schematic = Grid

type PartNumber = Int

type GearRatio = Int

test :: [String]
test =
  [ "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  ]

partNumbers :: Schematic -> [PartNumber]
partNumbers schem = (mapMaybe validParts . toList) schem
  where
    validParts :: ((Int, Int), GridElement) -> Maybe PartNumber
    validParts ((x, y), Number n) = if any isSymbol (neighbours (x, y) schem) then Just n else Nothing
    validParts (_, Symbol sym) = Nothing

gearRatios :: Schematic -> [GearRatio]
gearRatios schem = (mapMaybe ratios . toList) schem
  where
    ratios :: ((Int, Int), GridElement) -> Maybe GearRatio
    ratios ((x, y), Symbol sym) =
      case sym of
        Gear ->
          if length nums == 2
            then Just $ product (fmap getNumber nums)
            else Nothing
        _ -> Nothing
      where
        nums = filter isNumber (neighbours (x, y) schem)
    ratios _ = Nothing

getNumber :: GridElement -> Int
getNumber (Number n) = n

isNumber :: GridElement -> Bool
isNumber (Number _) = True
isNumber _ = False

isSymbol :: GridElement -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

loadSchematic :: String -> Schematic
loadSchematic = loadElements . zip [0 ..] . lines . replace '.' ' '

loadElements :: [(Int, String)] -> Schematic
loadElements whole = load whole 0
  where
    load :: [(Int, String)] -> Int -> Schematic
    load ((y, ' ' : rest) : lines) x = load ((y, rest) : lines) (x + 1)
    load ((y, s@(c : cs)) : lines) x =
      if isDigit c
        then case reads s of
          [(n, rest)] -> insert (x, y) num $ load ((y, rest) : lines) (x + width num)
            where
              num = number n
        else insert (x, y) (if c == '*' then gear else symbol) $ load ((y, cs) : lines) (x + 1)
    load ((_, []) : lines) x = load lines 0
    load [] _ = empty

day3_1 = show . sum . partNumbers . loadSchematic

day3_2 = show . sum . gearRatios . loadSchematic
