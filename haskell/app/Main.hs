module Main where
import System.Environment (getArgs)
import Day1 (day1_1, day1_2)

days :: [(String -> String, String -> String)]
days = [(day1_1, day1_2)] 

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