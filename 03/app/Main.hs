module Main where

-- needed imports
import Data.List as List
import Data.Map as Map

-- get a list of strings from a text file
snarfLines :: FilePath -> IO [String]

snarfLines name = do
  l <- readFile name
  let ll = lines l
  return ll

chunks :: String -> (String, String)
chunks backpack =
  List.splitAt ((length backpack) `div` 2) backpack

priMap :: Map Char Int
priMap = Map.fromList $ zip (['a'..'z'] ++ ['A'..'Z']) [1..]

commonLetter :: String -> String -> Char
commonLetter s1 s2 =
  head $ List.filter (`elem` s1) s2

priority :: Char -> Maybe Int
priority c =
  Map.lookup c priMap 


priCommon :: (String, String) -> Maybe Int
priCommon (s1, s2) =
  priority $ commonLetter s1 s2

commonLetter3 :: [String] -> Char
commonLetter3 triple =
  let
    [s1, s2, s3] = triple
    sub1 = List.filter (`elem` s1) s2
    commList = zip sub1 (List.map (`elem` s3) sub1)
  in
    fst $ head $ List.filter snd commList

processTriples :: [String] -> [Maybe Int]
processTriples strList =
  let
    (x,xs) = List.splitAt 3 strList
    in
    case (x,xs) of
      (a, []) -> [(priority $ commonLetter3 a)]
      (a,b) -> (priority $ commonLetter3 a) : processTriples b
  
  
main :: IO ()
main = do
  flines <- snarfLines "input"
--  nums <-  (mapM chunks) flines
  -- putStrLn $ show $ chunks $ head flines
  putStrLn $ "part 1: " ++ (show $ List.foldl' (+) 0 $ List.foldl' (+) 0 <$> priCommon <$> List.map (chunks) flines)
  putStrLn $ "part 2: " ++ (show $ List.foldl' (+) 0 $ List.foldl' (+) 0 <$> processTriples flines)
