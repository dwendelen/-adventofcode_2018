import Data.Bits
import Debug.Trace (trace)
import Data.Set

-- Advice: compile program with -O for fast result

main = print prog

prog =
  let
    d = 0
  in
    loop0 d empty

-- Keep looping until you find a number you have already seen
loop0 :: Int -> Set Int -> (Int, Int)
loop0 d nums =
  let
    e = d .|. 65536
    d2 = 1107552

    d3 = loop1 d2 e

    newNums = insert d3 nums
  in
    if member d3 nums
      then (d, size nums)
      else loop0 d3 newNums

loop1 :: Int -> Int -> Int
--loop1 d e | trace ("loop1 d: " ++ show d ++ " e: " ++ show e) False = undefined
loop1 d e =
  let
    f = e .&. 255
    d1 = d + f
    d2 = d1 .&. 16777215
    d3 = d2 * 65899
    d4 = d3 .&. 16777215

    e1 = loop2 e 0
  in
    if e < 256
      then d4
      else loop1 d4 e1

loop2 :: Int -> Int -> Int
loop2 e f =
  let
    b = f + 1
    b2 = b * 256
    f1 = f + 1
  in
    if b2 > e
      then f
      else loop2 e f1