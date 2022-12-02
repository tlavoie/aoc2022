module Main where

-- Likely-needed imports
import Data.List

-- get a list of strings from a text file
snarfLines :: FilePath -> IO [String]

snarfLines name = do
  l <- readFile name
  let ll = lines l
  return ll

scoreEntry :: String -> Int
scoreEntry oneround
  | oneround == "A X" = 1 + 3
  | oneround == "A Y" = 2 + 6
  | oneround == "A Z" = 3 + 0
  | oneround == "B X" = 1 + 0
  | oneround == "B Y" = 2 + 3
  | oneround == "B Z" = 3 + 6
  | oneround == "C X" = 1 + 6 
  | oneround == "C Y" = 2 + 0
  | oneround == "C Z" = 3 + 3
  | otherwise = 999 -- should never happen, but obvious if does

followStrategy :: String -> Int
followStrategy instr
  | instr == "A X" = scoreEntry "A Z"
  | instr == "A Y" = scoreEntry "A X"
  | instr == "A Z" = scoreEntry "A Y"
  | instr == "B X" = scoreEntry "B X"
  | instr == "B Y" = scoreEntry "B Y"
  | instr == "B Z" = scoreEntry "B Z"
  | instr == "C X" = scoreEntry "C Y"
  | instr == "C Y" = scoreEntry "C Z"
  | instr == "C Z" = scoreEntry "C X"
  | otherwise = 999 -- should never happen, but obvious if does


main :: IO ()
main = do
  flines <- snarfLines "input"
  putStrLn $ "part 1: " ++ (show $ foldl' (+) 0 $ map scoreEntry flines)
  putStrLn $ "part 2: " ++ (show $ foldl' (+) 0 $ map followStrategy flines)
