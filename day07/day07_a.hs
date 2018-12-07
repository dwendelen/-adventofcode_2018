import System.IO
import Text.Regex.Posix
import qualified Data.Map.Strict as Map
import Data.List (nub, sort, null, delete)

calc :: [(Char, Char)] -> [Char]
calc list =
  let
    letters = calcLetters list
    sorted = sort letters
    dependencies = calcDependencies list letters
    order = calcOrder sorted dependencies
  in
    order

calcLetters list =
  nub ( (map fst list ) ++ (map snd list))

calcDependencies :: [(Char, Char)] -> [Char] -> Map.Map Char [Char]
calcDependencies list letters =
  let
    initial = Map.fromList (map (\l -> (l, [])) letters)
  in
    foldl calcDependencies_ initial list

calcDependencies_ acc (from, to) =
  Map.update (\l -> Just (to : l)) from acc


calcOrder :: [Char] -> Map.Map Char [Char] -> [Char]
calcOrder todoSorted dependencies =
  calcOrder_ todoSorted dependencies []


calcOrder_ [] _ done = done
calcOrder_ todoSorted dependencies done =
  let
    nextLetter = head (filter (dependenciesAreDone todoSorted dependencies) todoSorted)
    newDone = done ++ [nextLetter]
    newTodo = delete nextLetter todoSorted
  in
    calcOrder_ newTodo dependencies newDone


dependenciesAreDone todoSorted dependencies letter =
  let
    deps = dependencies Map.! letter
    depsTodo = filter (\dep -> elem dep todoSorted) deps
  in
    null depsTodo

-- Util
main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    codes = lines file
    parsed = map parse codes
    result = calc parsed
  print result

parse :: String -> (Char, Char)
parse string =
  let
    (_,_,_, match) = string =~ "Step ([A-Z]) must be finished before step ([A-Z]) can begin." :: (String, String, String, [String])
    dependency = (match!!0)!!0
    depending = (match!!1)!!0
  in
    (depending, dependency)