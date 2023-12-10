module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day9
import System.Environment (getArgs)

days :: [(String -> String, String -> String)]
days =
  [ (day1_1, day1_2),
    (day2_1, day2_2),
    (day3_1, day3_2),
    (day4_1, day4_2),
    (day5_1, day5_2),
    (day6_1, day6_2),
    (day7_1, day7_2),
    (day6_1, day6_2),
    (day9_1, day9_2)
  ]

day :: String -> IO ()
day d = do
  putStr "Day "
  putStr d
  putStrLn " 2023!"
  s <- readFile ("../assets/day" ++ d ++ ".txt")
  putStrLn $ (fst $ days !! (read d - 1)) s
  putStrLn $ (snd $ days !! (read d - 1)) s

main :: IO ()
main = do
  args <- getArgs
  day $ head args