import System.IO
import Text.Regex.Posix

data Node = Node [Node] [Int] deriving (Show)

calc numbers =
  calcTree numbers

calcTree numbers =
  let
    (root, rest) = parseNode numbers
  in
    calcValue root

parseNode :: [Int] -> (Node, [Int])
parseNode (nbChildren : nbMeta : rest) =
  let
    (children, dataAfterChildren) = parseChildren nbChildren rest []
    meta = take nbMeta dataAfterChildren
    leftOver = drop nbMeta dataAfterChildren
  in
    (Node children meta, leftOver)

parseChildren 0 rest acc = (reverse acc, rest)
parseChildren n rest acc =
  let
    (child, afterChild) = parseNode rest
  in
    parseChildren (n - 1) afterChild (child : acc)

calcValue (Node children meta) =
  if null children
    then sum meta
    else
      let
        metaAsIdx = concatMap (\m -> if m == 0 then [] else [m - 1]) meta
        indexedChildren = concatMap (lookupChild children) metaAsIdx
        vals = map calcValue indexedChildren
      in
        sum vals

lookupChild children idx =
  if idx < length children
    then [children !! idx]
    else []

-- Util

main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    --codes = lines file
    --parsed = map parse codes
    parsed = parse file
    result = calc parsed
  print result

parse :: String -> [Int]
parse string =
  let
    numbers = words string
  in
    map (\n -> read n :: Int) numbers