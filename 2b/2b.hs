import Data.Char (isDigit)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes, fromJust, isNothing)

data Cube
  = Red Int
  | Blue Int
  | Green Int
  deriving (Show)

main = interact $ show . solve

solve input = sum setPowers
  where
    setPowers = map (powerOfSet . minSet . parseGame) $ lines input

powerOfSet (Red r, Green g, Blue b) = r * g * b

-- >>> minSet [Red 1, Blue 2, Green 3]
-- (Red 1,Green 4,Blue 2)
--
minSet :: [Cube] -> (Cube, Cube, Cube)
minSet = foldr minSet' (Red 0, Green 0, Blue 0)
  where
    minSet' c (r, g, b) =
      case c of
        Red n ->
          if n > getIntValue r
          then (Red n, g, b)
          else (r, g, b)
        Green n ->
          if n > getIntValue g
          then (r, Green n, b)
          else (r, g, b)
        Blue n ->
          if n > getIntValue b
          then (r, g, Blue n)
          else (r, g, b)

getIntValue :: Cube -> Int
getIntValue cube = case cube of
  Red x -> x
  Blue x -> x
  Green x -> x

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
