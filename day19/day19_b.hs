import qualified Data.Set as Set

{-
  The solution is the sum of all the factors of 10 551 381
-}

f :: Int
f = 10551381

main =
  print (foldl addIfFactor 0 [1..f])

addIfFactor acc i =
  if mod f i == 0
    then acc + i
    else acc