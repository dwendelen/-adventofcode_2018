import System.IO
import Text.Regex.Posix

import qualified Data.Set as Set


calc points =
  let
    (t, stable) = simulate 0 points (calcSpacing points)
  in
    printPoints stable
    
simulate t points previousSpacing =
  let
    newPoints = map moveOneStep points
    newSpacing = calcSpacing newPoints
  in
    if newSpacing > previousSpacing
      then (t, points)
      else simulate (t + 1) newPoints newSpacing

moveOneStep ((posX, posY), (velX, velY)) =
  ((posX + velX, posY + velY), (velX, velY))

calcSpacing points =
  let
    yVals = map (\((_, y), _) -> y) points
    min = minimum yVals
    max = maximum yVals
  in
    max - min

printPoints :: [((Int, Int), (Int, Int))] -> String
printPoints points =
  let
    yVals = map (\((_, y), _) -> y) points
    minY = minimum yVals
    maxY = maximum yVals
    sizeY = maxY - minY

    xVals = map (\((x, _), _) -> x) points
    minX = minimum xVals
    maxX = maximum xVals
    sizeX = maxX - minX

    normalisedPoints = normalisePoints points minX minY

    lines = map (printLine normalisedPoints [0..(sizeX)]) [0..(sizeY)]
  in
    foldl (\acc l -> acc ++ "\n" ++ l) "" lines

normalisePoints :: [((Int, Int), (Int, Int))] -> Int -> Int -> Set.Set (Int, Int)
normalisePoints points minX minY =
  let
    mapped = map (\((x, y), _) -> (x - minX, y - minY)) points
  in
    Set.fromList mapped

printLine :: Set.Set (Int, Int) -> [Int] -> Int -> String
printLine points xRange y =
  map (printChar points y) xRange

printChar points y x =
  if Set.member (x, y) points
    then '#'
    else ' '

-- Util
main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    codes = lines file
    parsed = map parse codes
    result = calc parsed
  putStr result

parse :: String -> ((Int, Int), (Int, Int))
parse string =
  let
    (_,_,_, match) = string =~ "position=< ?(-?[0-9]+),  ?(-?[0-9]+)> velocity=< ?(-?[0-9]+),  ?(-?[0-9]+)>" :: (String, String, String, [String])
    posX = read (match!!0) :: Int
    posY = read (match!!1) :: Int
    velX = read (match!!2) :: Int
    velY = read (match!!3) :: Int
  in
    ((posX, posY), (velX, velY))