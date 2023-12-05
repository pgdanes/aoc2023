import Data.Char

getNums :: String -> Int
getNums s =
  let nums = filter isNumber s in
  read [head nums, last nums]

main = interact $ show . sum . map getNums . lines

