{-# LANGUAGE TupleSections #-}

module Grid where

import Control.Monad (join)
import Data.Map as M (Map, insert, lookup, (!))
import Data.Maybe (mapMaybe)

type Grid = (Map (Int, Int) GridElement)

data Sym
  = Gear
  | Other
  deriving (Eq, Show)

data GridElement
  = Number Int
  | Symbol Sym
  deriving (Eq, Show)

width :: GridElement -> Int
width (Symbol _) = 1
width (Number n)
  | n < 10 = 1
  | n < 100 = 2
  | n < 10000 = 3

insert ::
  (Int, Int) -> -- x, y
  GridElement ->
  Grid ->
  Grid
insert pos@(x, y) = M.insert pos

empty :: Grid
empty = mempty

number :: Int -> GridElement
number = Number

symbol :: GridElement
symbol = Symbol Other

gear :: GridElement
gear = Symbol Gear

get ::
  (Int, Int) ->
  Grid ->
  Maybe GridElement
get = M.lookup

get' ::
  (Int, Int) ->
  Grid ->
  GridElement
get' = flip (M.!)

neighbours ::
  (Int, Int) ->
  Grid ->
  [GridElement]
neighbours pos@(x, y) g =
  case get pos g of
    Just el@(Number n) ->
      mapMaybe (`get` g) ([(x - 1, y), (x + w, y)] ++ map (,y - 1) xs ++ map (,y + 1) xs)
      where
        xs = [x - 1 .. x + w]
        w = width el
    Just el@(Symbol sym) ->
      mapMaybe symbolNeighbours (map (,y) [x - 3 .. x - 1] ++ [(x + 1, y)] ++ map (,y - 1) [x - 3 .. x + 1] ++ map (,y + 1) [x - 3 .. x + 1])
      where
        symbolNeighbours pos = if el `elem` neighbours pos g then Just (get' pos g) else Nothing
    Nothing -> []
