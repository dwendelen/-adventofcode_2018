import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    result = calc file
  print result


calc :: [Char] -> Int
calc list =
  let
    reduced = reduce [] list
  in
    length reduced

reduce :: [Char] -> [Char] -> [Char]
reduce [] (r: rs) =
  reduce [r] rs

reduce l [] = reverse l

reduce (l: ls) (r: rs) =
  if cancelEachOtherOut l r
    then reduce ls rs
    else reduce (r:l:ls) rs

cancelEachOtherOut a b =
  a /= b && (toUpper a) == (toUpper b)