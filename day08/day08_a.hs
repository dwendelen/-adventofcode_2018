import System.IO
import Text.Regex.Posix

data Node = Node [Node] [Int] deriving (Show)

calc numbers =
  calcTree numbers

calcTree numbers =
  let
    (root, rest) = parseNode numbers
  in
    calcMetaSum root

parseNode :: [Int] -> (Node, [Int])
parseNode (nbChildren : nbMeta : rest) =
  let
    (children, dataAfterChildren) = parseChildren nbChildren rest []
    meta = take nbMeta dataAfterChildren
    leftOver = drop nbMeta dataAfterChildren
  in
    (Node children meta, leftOver)

parseChildren 0 rest acc = (acc, rest)
parseChildren n rest acc =
  let
    (child, afterChild) = parseNode rest
  in
    parseChildren (n - 1) afterChild (child : acc)

calcMetaSum (Node children meta) =
  let
    myMeta = sum meta
    childrenSum = sum ( map calcMetaSum children)
  in
    myMeta + childrenSum

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