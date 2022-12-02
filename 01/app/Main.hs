module Main where

-- Likely-needed imports
import Data.List

-- get a list of strings from a text file
snarfLines :: FilePath -> IO [String]
snarfLines name = do
  l <- readFile name
  let ll = lines l
  return ll

gather :: [String] -> [Int] -> [[Int]] -> [[Int]]
gather (x:xs) single full | x /= "" = gather xs ((read x :: Int) : single) full
                          | x == "" = gather xs [] (single : full)
gather _ single full = single : full


main :: IO ()
main = do
  flines <- snarfLines "input"
  let g = gather flines [] []
      totals = map (foldl' (+) 0) g
    in do
    putStrLn $ "part 1: " ++ (show $ maximum totals)
    putStrLn $ "part 2: " ++ (show $ foldr (+) 0 $ take 3 $ reverse $ sort totals)
