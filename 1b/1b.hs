import Data.Char (digitToInt, isDigit)
import Data.Foldable (find)
import Data.List (isPrefixOf, tails)
import Data.Maybe (catMaybes, listToMaybe)

multiMap :: [a -> b] -> a -> [b]
multiMap fs x =
  multiMap' fs x []
  where
    multiMap' [] x acc = acc
    multiMap' fs x acc =
      multiMap' restFns x (currentFn x : acc)
      where
        restFns = tail fs
        currentFn = head fs

parseNums :: String -> [Int]
parseNums = concatMap parseNum . tails

parseNum :: String -> [Int]
parseNum = catMaybes . multiMap rules
  where
    parseN prefix num string =
      if prefix `isPrefixOf` string then Just num else Nothing
    parseCharNum =
      fmap digitToInt . find isDigit . listToMaybe
    rules =
      [ parseCharNum,
        parseN "one" 1,
        parseN "two" 2,
        parseN "three" 3,
        parseN "four" 4,
        parseN "five" 5,
        parseN "six" 6,
        parseN "seven" 7,
        parseN "eight" 8,
        parseN "nine" 9
      ]

parseNumLine :: String -> Int
parseNumLine line = startNum * 10 + endNum
  where
    numbers = parseNums line
    startNum = head numbers
    endNum = last numbers

main = interact $ show . sum . map parseNumLine . lines
