import Data.Map.Strict as Map
import Data.Set as Set
import Data.Maybe
import Debug.Trace (trace)

--depth = 510
--targetX = 10
--targetY = 10

depth = 9171
targetX = 7
targetY = 721

data Type = Rocky | Wet | Narrow deriving (Eq, Ord, Show)
data Tool = Torch | Climbing | Neither deriving (Eq, Ord, Show)

main =
  print calc

calc =
  let
    initialCache = Map.empty
    initial = (0, 0, Torch)
  in
    tick Set.empty [initial] Map.empty 0 initialCache

target = (targetX, targetY, Torch)

-- Nodes
type Node = (Int, Int, Tool)

getPosFromNode (x, y, _) = (x, y)

tick :: Set Node -> [Node] -> Map Int [Node] -> Int -> Cache -> Int
--tick knownNodes currentSlot slots t cache | trace ("tick " ++ show t ++ ": " ++ show currentSlot) False = undefined
tick knownNodes currentSlot slots t cache
  | elem target currentSlot = t
  | otherwise =
      let
        (newKnown, slots1, newCache) = Prelude.foldl (handleNode t) (knownNodes, slots, cache) currentSlot
        (nextT, nextSlot) = fromJust (Map.lookupGT t slots1)
        slots2 = Map.delete nextT slots1
      in
        tick newKnown nextSlot slots2 nextT newCache

handleNode :: Int -> (Set Node, Map Int [Node], Cache) -> Node -> (Set Node, Map Int [Node], Cache)
handleNode t (known, slots, cache) node
  | Set.member node known = (known, slots, cache)
  | otherwise =
      let
        (inValid, newCache) = isInvalid cache node
      in
        if inValid
          then (known, slots, newCache)
          else handleValidNode t known node newCache slots

handleValidNode :: Int -> Set Node -> Node -> Cache -> Map Int [Node] -> (Set Node, Map Int [Node], Cache)
handleValidNode t known node cache slots =
  let
    (typ, _) = getType cache (getPosFromNode node)
    newKnown = Set.insert node known
    extraSlots = nextMoves t typ node
    newSlots = unionWith (++) extraSlots slots
  in
    (newKnown, newSlots, cache)

nextMoves t typ (x, y, tool) =
  Map.fromList [
    (t + 7, [(x, y, otherTool typ tool)]),
    (t + 1, [
      (x - 1, y, tool),
      (x + 1, y, tool),
      (x, y - 1, tool),
      (x, y + 1, tool)
    ])
  ]

isInvalid :: Cache -> Node -> (Bool, Cache)
isInvalid cache (x, y, tool)
  | x < 0 = (True, cache)
  | y < 0 = (True, cache)
  | otherwise =
      let
        (myType, newCache) = getType cache (x, y)
      in
        (invalidTool myType tool, newCache)

otherTool Rocky Climbing = Torch
otherTool Rocky Torch = Climbing
otherTool Wet Climbing = Neither
otherTool Wet Neither = Climbing
otherTool Narrow Torch = Neither
otherTool Narrow Neither = Torch

invalidTool Rocky Neither = True
invalidTool Wet Torch = True
invalidTool Narrow Climbing = True
invalidTool _ _ = False

-- Calculating Types

type Cache = Map (Int, Int) Int

getType :: Cache -> (Int, Int) -> (Type, Cache)
getType erosionCache pos =
  let
    (ero, newCache) = getEro erosionCache pos
  in
    (calcType ero, newCache)

getEro erosionCache pos =
  case Map.lookup pos erosionCache of
      Just ero -> (ero, erosionCache)
      Nothing -> let
          (newEro, cache1) = calcEro erosionCache pos
          newCache = Map.insert pos newEro cache1
        in
          (newEro, newCache)

calcEro erosionCache pos =
  let
    (geo, cache1) = calcGeo erosionCache pos
    ero = mod (geo + depth) 20183
  in
    (ero, cache1)

calcGeo :: Cache -> (Int, Int) -> (Int, Cache)
--calcGeo cache pos | trace (show pos) False = undefined
calcGeo erosionCache (x, y)
  | x == 0 && y == 0 = (0, erosionCache)
  | x == targetX && y == targetY = (0, erosionCache)
  | y == 0 = (x * 16807, erosionCache)
  | x == 0 = (y * 48271, erosionCache)
  | otherwise =
    let
      (up, cache1) = getEro erosionCache (x, y - 1)
      (left, cache2) = getEro cache1 (x - 1, y)
    in
      (up * left, cache2)

calcType :: Int -> Type
calcType ero =
  case mod ero 3 of
    0 -> Rocky
    1 -> Wet
    2 -> Narrow

--For debugging

foldFunc (acc, cache) (y, x) =
  let
    (typ, newCache) = getType cache (x, y)
    char = case typ of
      Rocky -> '.'
      Wet -> '='
      Narrow -> '|'
    newAcc = acc ++ [char]
  in
    (newAcc, newCache)

-- End for debugging