import System.IO
import Data.List
import Text.Regex.Posix
import Data.Maybe
import qualified Data.Map.Strict as Map

data Code = Code { cid:: Integer
                 , pos:: (Integer, Integer)
                 , size:: (Integer, Integer)
                 } deriving (Show)


main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    codes = lines file
    parsed = Data.List.map parse codes
    result = calc parsed
  print result


calc :: [Code] -> Int
calc list =
  let
    cells = foldl incrCells Map.empty list
    filtered = Map.filter (\c -> c > 1) cells
  in
    Map.size filtered

incrCells acc code =
  foldl incrCell acc (getGrid code)

incrCell acc cell =
  Map.alter incr cell acc

incr :: Maybe Integer -> Maybe Integer
incr old =
  let
    newVal = case old of Nothing -> 1
                         Just a -> a + 1
  in
    Just newVal

getGrid Code {cid=_, pos=(posX, posY), size=(sizeX, sizeY)} =
  [(x, y) | x <- [posX .. (posX + sizeX - 1)], y <- [posY .. (posY + sizeY - 1)]]

parse :: String -> Code
parse string =
  let
    (_,_,_, match) = string =~ "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)" :: (String, String, String, [String])
    cid = read (match!!0) :: Integer
    posX = read (match!!1) :: Integer
    posY = read (match!!2) :: Integer
    sizeX = read (match!!3) :: Integer
    sizeY = read (match!!4) :: Integer
  in
    Code { cid = cid
         , pos = (posX, posY)
         , size = (sizeX, sizeY)
         }
