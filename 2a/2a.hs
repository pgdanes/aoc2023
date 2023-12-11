import Data.Char (isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes, fromJust, isNothing)

data Cube
  = Red Int
  | Blue Int
  | Green Int
  deriving (Show)

main = interact $ show . solve

solve input = sum (map fst numberedValids)
  where 
    numberedLines = zip [1..] (map (isValidGame . parseGame) $ lines input)
    numberedValids = filter snd numberedLines

isValidGame :: [Cube] -> Bool
isValidGame = foldr isValid True
  where
    isValid = (&&) . isValidCube 12 13 14

isValidCube :: Int -> Int -> Int -> Cube -> Bool
isValidCube maxR maxG maxB cube =
  case cube of
    Red i -> i <= maxR
    Green i -> i <= maxG
    Blue i -> i <= maxB

parseGame line = reverse . catMaybes $ parseCubeLine line []
  where
    parseCubeLine "" acc = acc
    parseCubeLine (char : rest) acc
      | isDigit char = parseCubeLine rest (parseCube line : acc)
      | otherwise = parseCubeLine rest acc
      where
        line = char : rest

parseCube :: String -> Maybe Cube
parseCube line
  | isNothing rest = Nothing
  | " red" `isPrefixOf` fromJust rest = Just (Red num)
  | " blue" `isPrefixOf` fromJust rest = Just (Blue num)
  | " green" `isPrefixOf` fromJust rest = Just (Green num)
  | otherwise = Nothing
  where
    nums = takeWhile isDigit line
    num = read nums :: Int
    rest = stripPrefix nums line
