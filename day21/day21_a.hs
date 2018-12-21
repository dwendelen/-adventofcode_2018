import Data.Bits
import Debug.Trace (trace)

main = print prog

prog =
  let
    d = 1107552
    e = 65536
  in
    loop1 d e

loop1 :: Int -> Int -> Int
loop1 d e | trace ("loop d: " ++ show d ++ " e: " ++ show e) False = undefined
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