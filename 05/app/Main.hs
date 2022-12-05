module Main where

-- needed imports
import Data.List as List
import Data.Array as Array

-- get a list of strings from a text file
snarfLines :: FilePath -> IO [String]

snarfLines name = do
  l <- readFile name
  let ll = drop 10 $ lines l
  return ll

--         [J]         [B]     [T]    
--         [M] [L]     [Q] [L] [R]    
--         [G] [Q]     [W] [S] [B] [L]
-- [D]     [D] [T]     [M] [G] [V] [P]
-- [T]     [N] [N] [N] [D] [J] [G] [N]
-- [W] [H] [H] [S] [C] [N] [R] [W] [D]
-- [N] [P] [P] [W] [H] [H] [B] [N] [G]
-- [L] [C] [W] [C] [P] [T] [M] [Z] [W]
--  1   2   3   4   5   6   7   8   9 

-- hard-coding starting lists
l1 = ['D','T','W','N','L']
l2 = ['H','P','C']
l3 = ['J','M','G','D','N','H','P','W']
l4 = ['L','Q','T','N','S','W','C']
l5 = ['N','C','H','P']
l6 = ['B','Q','W','M','D','N','H','T']
l7 = ['L','S','G','J','R','B','M']
l8 = ['T','R','B','V','G','W','N','Z']
l9 = ['L','P','N','D','G','W']

initBlockList :: [[Char]]
initBlockList = [l1,l2,l3,l4,l5,l6,l7,l8,l9]

arrSize :: Int
arrSize = length initBlockList

initBlocks :: Array Int [Char]
initBlocks = listArray (1,arrSize) initBlockList

-- example move: "move 6 from 6 to 5"

notIn :: (Foldable t, Eq a) => t a -> a -> Bool
notIn arr ch = notElem ch arr

-- Reusing split from last time
wc :: Foldable t => [[Char]] -> t Char -> [Char] -> [[Char]]
wc l _ "" = reverse l
wc l sp s = do
  let
    st = span (notIn sp) s
  wc ((fst st):l) sp (drop 1 (snd st))  

-- This is the nice string-chunker, using wc above
split :: String -> String -> [String]
split sp s = wc [] sp s
                     
moveFromStr :: String -> (Int,Int,Int)
moveFromStr str =
  let
    chunks = split " " str
  in
    case chunks of
      [_,count,_,src,_,dst] -> ((read count :: Int),
                                (read src :: Int),
                                (read dst :: Int))
      _ -> (0,0,0)

applyMove :: (Int,Int,Int) -> Array Int [Char] -> Bool -> Array Int [Char]
applyMove move blocks invert =
  let
    (count,src,dst) = move
    (taken,left) = List.splitAt count (blocks ! src)
  in
    case invert of
      True -> blocks // [(src, left), (dst, (reverse taken ++ (blocks ! dst)))]
      False -> blocks // [(src, left), (dst, (taken ++ (blocks ! dst)))]
    

applyAllMoves :: [String] -> Array Int [Char] -> Bool -> Array Int [Char]
applyAllMoves moveList blocks invert =
  let
    (x:xs) = moveList
  in
    case xs of
      [] -> applyMove (moveFromStr x) blocks invert
      _ -> applyAllMoves xs (applyMove (moveFromStr x) blocks invert) invert


  
main :: IO ()
main = do
  flines <- snarfLines "input"
  putStrLn $  elems $ fmap head $ applyAllMoves flines initBlocks True
  putStrLn $  elems $ fmap head $ applyAllMoves flines initBlocks False


