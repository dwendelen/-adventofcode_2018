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
    abc = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
           'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']``
    mapped = map (test list) abc
  in
    minimum mapped

test list without =
  let
    filtered = filter (\c -> c /= without && c /= (toUpper without)) list
    reduced = reduce [] filtered
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