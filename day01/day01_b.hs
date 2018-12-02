import System.IO
import Data.Set

main :: IO ()
main = do
  file <- readFile "input.txt";
  let ints = Prelude.map parse (lines file);
  let res = calc ints;
  print res

calc :: [Integer] -> Integer
calc ints =
  let drifts = tail (scanl (+) 0 ints) in
  let set = fromList drifts in
  let step = last drifts in
  solve drifts drifts set step step

solve :: [Integer] -> [Integer] -> Set Integer -> Integer -> Integer -> Integer
solve [] drifts set offset step =
  solve drifts drifts set (offset + step) step
solve (int : other) drifts set offset step =
  let val = int + offset in
  if member val set
    then val
    else solve other drifts set offset step


parse :: String -> Integer
parse str =
  let withoutPlus = removePlus str in
  read withoutPlus::Integer

removePlus :: [Char] -> [Char]
removePlus [] = []
removePlus (c:cs) =
  if c == '+'
    then cs
    else (c:cs)