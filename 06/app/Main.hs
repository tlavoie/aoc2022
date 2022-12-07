module Main where

-- needed imports
import qualified Data.ByteString as B
import Data.List as L

-- get a list of strings from a text file
snarf :: FilePath -> IO B.ByteString
snarf name = do
  l <- B.readFile name
  return l

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

-- take a short ByteString (length 4)
-- For each position other than its own, check for equality
-- Mapping positions 3..n-1 for length n, use indices from
-- pairs [-3..0]

getOffset :: Int -> (Int,Int) -> (Int,Int)
getOffset x (i,j) =
  (x + i, x + j)

indexCombos :: Int -> Int -> [(Int,Int)]
indexCombos x size =
  let
    p = pairs [(1- size) .. 0]
  in
    map (getOffset x) p

-- Checks one pair of entries in a ByteString
checkOnePair :: B.ByteString -> (Int,Int) -> Bool
checkOnePair bs (i,j) =
  B.index bs i /= B.index bs j

-- Are all the pairs from a given index different?
checkAllDifferent :: B.ByteString -> Int -> Int -> Bool
checkAllDifferent bs size index =
  let
    cp = checkOnePair bs  -- note partial application
    pairList = indexCombos index size
  in
    L.all cp pairList

subByteString :: B.ByteString -> Int -> Int -> B.ByteString
subByteString bs offset len =
  B.take len $ B.drop offset bs
  
main :: IO ()
main = do
  input <- snarf "input"
  putStrLn $ show input
  let
    checkSize1 = 4
    i1 = [(checkSize1 - 1) .. B.length input]
    x1 = L.findIndex (checkAllDifferent input checkSize1) i1
    in 
    putStrLn $ "part 1: " ++ show (fmap (+ checkSize1 ) x1);

  let
    checkSize2 = 14
    i2 = [(checkSize2 - 1) .. B.length input]
    x2 = L.findIndex (checkAllDifferent input checkSize2) i2
    in 
    putStrLn $ "part 2: " ++ show (fmap (+ checkSize2 ) x2) -- 
