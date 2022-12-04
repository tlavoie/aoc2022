module Main where

-- needed imports
--import Data.List as List
--import Data.Map as Map

-- get a list of strings from a text file
snarfLines :: FilePath -> IO [String]

snarfLines name = do
  l <- readFile name
  let ll = lines l
  return ll

-- Added, just so that there was a version of notElem which would fit
-- nicely into what is expected by the span function.
notIn :: (Foldable t, Eq a) => t a -> a -> Bool
notIn arr ch = notElem ch arr


-- Called with three parameters: One string to split, one string
-- containing characters on which to split, plus one empty starting
-- list of words. Returns the list of delimited words.

wc :: Foldable t => [[Char]] -> t Char -> [Char] -> [[Char]]
wc l _ "" = reverse l
wc l sp s = do
  let
    st = span (notIn sp) s
  wc ((fst st):l) sp (drop 1 (snd st))  

-- This is the nice string-chunker, using wc above
split :: String -> String -> [String]
split sp s = wc [] sp s
                     

-- Checker to see if a single range is ever reversed (larger-smaller)
-- They aren't in the examples, quick to write for now though.
checkLargeSmall :: Int -> Int -> Bool
checkLargeSmall start end =
  start > end

-- Takes input like "8-41,8-79", returns ["8","41","8","79"]
parseInputLine :: String -> ((Int,Int),(Int,Int))
parseInputLine str =
  let
    assignments = split "-," str
  in
    case assignments of
      [a,b,c,d] -> (((read a :: Int),
                     (read b :: Int)),
                     ((read c :: Int),
                      (read d :: Int)))
      _ -> ((0,0),(0,0))
           -- should never happen, but is at
           -- least obvious, if not
           -- type-checked as a Maybe if we
           -- were doing this with unreliable
           -- input

hasSubset :: ((Int,Int),(Int,Int)) -> Bool
hasSubset ((a,b),(c,d)) =
  (((a <= c) && (b >= d)) || ((c <= a) && (d >= b)))

hasOverlap :: ((Int,Int),(Int,Int)) -> Bool
hasOverlap ((a,b),(c,d)) =
  (((a <= c) && (b >= c)) || ((c <= a) && (d >= a)))

  
  
main :: IO ()
main = do
  flines <- snarfLines "input"
--  nums <-  (mapM chunks) flines
  -- putStrLn $ show $ chunks $ head flines
  putStrLn $ "part 1: " ++ (show $ length $ filter id $ map (hasSubset . parseInputLine) flines)
  putStrLn $ "part 2: " ++ (show $ length $ filter id $ map (hasOverlap . parseInputLine) flines)
