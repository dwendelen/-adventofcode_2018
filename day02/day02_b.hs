import System.IO
import Data.List
import qualified Data.Map.Strict as Map


main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    codes = lines file
    result = calc codes
  print result


calc :: [String] -> String
calc list =
  findMatch list list

findMatch [] list =
  "Nothing found"
findMatch (x : xs) list =
  let
    found = findMatch1 list x
  in
    if found == ""
      then findMatch xs list
      else found

findMatch1 [] _ =
  ""
findMatch1 (x: xs) toMatch =
  let
    found = findMatch2 x toMatch ""
  in
    if found == ""
      then findMatch1 xs toMatch
      else found

findMatch2 :: [Char] -> [Char] -> [Char] -> [Char]
findMatch2 [] _ _ = --Ignore exact match
  ""
findMatch2 _ [] _ = --Ignore exact match
  ""
findMatch2 (x : xs) (y : ys) acc =
  if x == y
  then findMatch2 xs ys (acc ++ [x])
  else findMatch3 xs ys acc

findMatch3 [] _ acc =
  acc
findMatch3 _ [] acc =
  acc
findMatch3 (x : xs) (y : ys) acc =
  if x == y
  then findMatch3 xs ys (acc ++ [x])
  else ""