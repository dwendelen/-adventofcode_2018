import qualified Data.Map.Strict as Map

--nbOfPlayers = 9
--highest = 25

--nbOfPlayers = 10
--highest = 1618

--nbOfPlayers = 13
--highest = 7999

--nbOfPlayers = 17
--highest = 1104

--nbOfPlayers = 21
--highest = 6111

--nbOfPlayers = 30
--highest = 5807

nbOfPlayers = 416
highest = 71975

main =
  print calc

calc =
  let
    initialScores = map (\p -> (p, 0)) [0..(nbOfPlayers-1)]
    scores = play [0] 0 1 0 (Map.fromList initialScores)
    max = maximum (map snd (Map.toList scores))
  in
    max

play marbles currentIdx marble player scores
  | marble > highest      = scores
  | (mod marble 23) == 0  =
      let
        (newMarbles, newIdx, newScore) = removeMarbles marbles currentIdx marble player scores
        nextPlayer = mod (player + 1) nbOfPlayers
      in
        play newMarbles newIdx (marble + 1) nextPlayer newScore
  | otherwise             =
      let
        (newMarbles, newIdx) = insertMarble marbles currentIdx marble
        nextPlayer = mod (player + 1) nbOfPlayers
      in
        play newMarbles newIdx (marble + 1) nextPlayer scores

insertMarble [] _ marble =
    ([marble], 0)
insertMarble marbles currentIdx marble =
  let
    splitPoint = mod (currentIdx + 2) (length marbles)
    (beforeInsert, afterInsert) = splitAt splitPoint marbles
    newMarbles = beforeInsert ++ [marble] ++ afterInsert
    newIdx = splitPoint
  in
    (newMarbles, newIdx)

removeMarbles marbles currentIdx marble player scores =
  let
    splitPoint = mod (currentIdx - 7) (length marbles)
    (beforeRemove, (toRemove: afterRemove)) = splitAt splitPoint marbles
    pointsWon = marble + toRemove
    newScores = Map.update (\s -> Just (s + pointsWon)) player scores
  in
    (beforeRemove ++ afterRemove, splitPoint, newScores)
