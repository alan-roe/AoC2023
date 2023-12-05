module Main where
import System.Environment (getArgs)
import Day1 (day1_1, day1_2)
import Day2 (day2_1, day2_2)
import Day3 (day3_1, day3_2)
import Day4 (day4_1, day4_2)

days :: [(String -> String, String -> String)]
days = [(day1_1, day1_2), (day2_1, day2_2), (day3_1, day3_2), (day4_1, day4_2)] 

day :: String -> IO()
day d = do
  putStr "Day "
  putStr d
  putStrLn " 2023!"
  s <- readFile ("../assets/day" ++ d ++ ".txt")
  putStrLn $ (fst $ days !! (read d-1)) s
  putStrLn $ (snd $ days !! (read d-1)) s
    
main :: IO ()
main = do
  args <- getArgs
  day $ head args