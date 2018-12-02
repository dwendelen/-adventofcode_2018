import System.IO

main :: IO ()
main = do
  file <- readFile "input.txt";
  let result = readAndSum (lines file) 0
  print result

readAndSum :: [String] -> Integer -> Integer
readAndSum [] acc =
  acc
readAndSum (l: ls) acc =
  readAndSum ls (acc + (read (removePlus l)::Integer))

removePlus :: [Char] -> [Char]
removePlus [] = []
removePlus (c:cs) =
  if c == '+'
  then cs
  else (c:cs)