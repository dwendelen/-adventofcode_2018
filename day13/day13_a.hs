import System.IO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Data.List (sortBy)

data Tile   = Normal
            | Intersection
            | CornerNW_SE -- /
            | CornerNE_SW -- \
            deriving (Eq, Show, Ord)

data Orientation  = North
                  | East
                  | South
                  | West
                  deriving (Eq, Show, Ord)

data Rotation = RotLeft
              | Straight
              | RotRight
              deriving (Eq, Show, Ord)

data Cart = Cart (Int, Int) Orientation Rotation
  deriving (Eq, Show, Ord)

getPos (Cart pos _ _) = pos

calc tiles carts =
  tickLoop tiles carts

tickLoop _ carts | trace ("tick " ++ show carts) False = undefined
tickLoop tiles carts =
  let
    sortedCarts = sortBy playOrder (Set.toList carts)
    (collision, newCarts) = tickCarts tiles sortedCarts carts
  in
    case collision of
      Nothing -> tickLoop tiles newCarts
      Just c -> c

playOrder (Cart (x0, y0) _ _) (Cart (x1, y1) _ _) =
  if y0 == y1 then
    compare x0 x1
  else
    compare y0 y1

tickCarts tiles [] allCarts = (Nothing, allCarts)
tickCarts tiles (cart: carts) allCarts =
  let
    otherCarts = Set.delete cart allCarts
    newCart = tickCart tiles cart
    newAllCarts = Set.insert newCart otherCarts
  in
    if detectedCollision otherCarts newCart
    then (Just (getPos newCart), newAllCarts)
    else tickCarts tiles carts newAllCarts


detectedCollision otherCarts (Cart cartPos _ _) =
  let
    colliding = Set.filter (\(Cart pos _ _) -> pos == cartPos) otherCarts
  in
    not (null colliding)

tickCart tiles (Cart (x, y) orientation nextIntersectionOrientation) =
  let
    newPos = case orientation of
      North -> (x, y - 1)
      East -> (x + 1, y)
      South -> (x, y + 1)
      West -> (x - 1, y)
    tile = tiles Map.! newPos
    (newOrientation, newNextIntersectionOrientation) =
      case tile of
        Normal -> (orientation, nextIntersectionOrientation)
        Intersection -> handleIntersection orientation nextIntersectionOrientation
        CornerNW_SE -> (handleNW_SE orientation ,nextIntersectionOrientation)
        CornerNE_SW -> (handleNE_SW orientation ,nextIntersectionOrientation)
    newCart = Cart newPos newOrientation newNextIntersectionOrientation
  in
    newCart

handleIntersection North RotLeft = (West, Straight)
handleIntersection East RotLeft = (North, Straight)
handleIntersection South RotLeft = (East, Straight)
handleIntersection West RotLeft = (South, Straight)

handleIntersection North Straight = (North, RotRight)
handleIntersection East Straight = (East, RotRight)
handleIntersection South Straight = (South, RotRight)
handleIntersection West Straight = (West, RotRight)

handleIntersection North RotRight = (East, RotLeft)
handleIntersection East RotRight = (South, RotLeft)
handleIntersection South RotRight = (West, RotLeft)
handleIntersection West RotRight = (North, RotLeft)

handleNE_SW North = West
handleNE_SW East = South
handleNE_SW South = East
handleNE_SW West = North

handleNW_SE North = East
handleNW_SE East = North
handleNW_SE South = West
handleNW_SE West = South




-- Util
main :: IO ()
main = do
  file <- readFile "input.txt";
  let
    rows = lines file
    (tiles, carts) = parse rows
    result = calc tiles carts
  print result


parse :: [[Char]] -> (Map.Map (Int, Int) Tile, Set.Set Cart)
parse rows =
  parseRows rows 0 (Map.empty, Set.empty)


parseRows [] _ acc = acc
parseRows (r: rs) y acc =
  let
    newAcc = parseTiles r 0 y  acc
  in
    parseRows rs (y + 1) newAcc

parseTiles [] _ _ acc = acc
parseTiles (t: ts) x y (tiles, carts) =
  let
    newTile = parseTile t
    newCart = parseCart x y t
    newTiles = case newTile of
      Just tile -> Map.insert (x, y) tile tiles
      Nothing -> tiles
    newCarts = case newCart of
      Just cart -> Set.insert cart carts
      Nothing -> carts
  in
    parseTiles ts (x + 1) y (newTiles, newCarts)


parseTile '|'  = Just Normal
parseTile '-'  = Just Normal
parseTile '^'  = Just Normal
parseTile 'v'  = Just Normal
parseTile '<'  = Just Normal
parseTile '>'  = Just Normal
parseTile '/'  = Just CornerNW_SE
parseTile '\\' = Just CornerNE_SW
parseTile '+'  = Just Intersection
parseTile  _   = Nothing

parseCart x y '^' = Just (Cart (x, y) North RotLeft)
parseCart x y '>' = Just (Cart (x, y) East RotLeft)
parseCart x y 'v' = Just (Cart (x, y) South RotLeft)
parseCart x y '<' = Just (Cart (x, y) West RotLeft)
parseCart _ _  _  = Nothing