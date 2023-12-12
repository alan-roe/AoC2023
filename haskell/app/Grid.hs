{-# LANGUAGE RankNTypes #-}
module Grid where

import Data.Map as M (Map, elems, filterWithKey, insert, lookup, (!))

type Grid a = GridElement a => Map (Int, Int) a

class GridElement a where
  width :: a -> Int

insert ::
  (GridElement a) =>
  (Int, Int) -> -- x, y
  a ->
  Grid a ->
  Grid a
insert pos@(x, y) = M.insert pos

empty :: (GridElement a) => Grid a
empty = mempty

get ::
  (GridElement a) =>
  (Int, Int) ->
  Grid a ->
  Maybe a
get = M.lookup

get' ::
  (GridElement a) =>
  (Int, Int) ->
  Grid a ->
  a
get' = flip (M.!)

neighbours ::
  (GridElement a) =>
  (Int, Int) ->
  Grid a ->
  [a]
neighbours pos@(x, y) g = elems $ filterWithKey neighbourFilter g
  where
    neighbourFilter :: (GridElement a) => (Int, Int) -> a -> Bool
    neighbourFilter npos@(nx, ny) nel =
      npos /= pos
        && (ny == y - 1 || ny == y || ny == y + 1)
        && any (`elem` [x - 1 .. x + width el]) [nx .. nx + width nel - 1]
    el = get' pos g
