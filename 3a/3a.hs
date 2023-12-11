import Data.Char ( isNumber )
import Data.Map ( Map, fromList, findWithDefault, (!) )
import GHC.Exts.Heap.FFIClosures (parseTsoFlags)

example :: String
example =
  "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

data SchematicPart
  = -- Symbol (Coord)
    Symbol (Int, Int) Char
  | -- PartNumber (Start, End, Value)
    PartNumber (Int, Int) (Int, Int) Int
  deriving (Show)

isSymbol :: SchematicPart -> Bool
isSymbol (Symbol {}) = True
isSymbol _ = False

isPartNumber :: SchematicPart -> Bool
isPartNumber (PartNumber {}) = True
isPartNumber _ = False

coordsAroundPart :: SchematicPart -> [(Int, Int)]
coordsAroundPart (Symbol (x, y) _) = 
  [(i, j) | i <- [x-1..x+1], j <- [y-1, y+1]] 
coordsAroundPart _ = []

parseSchematic :: String -> [SchematicPart]
parseSchematic s = parse s (0, 0) []
  where
    parse "" _ acc = acc
    parse (char : rest) (x, y) acc
      | char == '\n' = parse rest (0, y + 1) acc
      | char == '.' = parse rest (x + 1, y) acc
      | isNumber char =
          let (numChars, remaining) = span isNumber (char : rest)
              len = length numChars
              part = PartNumber (x, y) (x + (len-1), y) (read numChars)
           in parse (drop (len-1) rest) (x + len, y) (part : acc)
      | otherwise =
          let symbol = Symbol (x - 1, y) char
           in parse rest (x + 1, y) (symbol : acc)

-- Constructs a map of schematic parts
-- PartNumbers are given multiple keys as they span multiple row/cols
fromSchematicList :: [SchematicPart] -> Map (Int, Int) SchematicPart
fromSchematicList parts = fromList keyedList
  where
    toKeyedPart  (Symbol coords char) = [(coords, Symbol coords char)]
    toKeyedPart (PartNumber (x, y) (w, z) value) =
      map (, PartNumber (x, y) (w, z) value) allCoords
      where
        allCoords = [(i, j) | i <- [x..w], j <- [y..z]]
    keyedList = concatMap toKeyedPart parts

-- schematicList = parseSchematic example
-- schematicMap = fromSchematicList schematicList
--
-- keepValidPartNumbers :: [SchematicPart] -> [SchematicPart]
-- keepValidPartNumbers originalParts =
--   originalParts
--   where
--     partNumbers = filter isPartNumber originalParts
--     partMap = fromSchematicList partNumbers
--     symbols = filter isSymbol originalParts

