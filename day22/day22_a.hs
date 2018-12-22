--depth = 510
--targetX = 10
--targetY = 10

depth = 9171
targetX = 7
targetY = 721



main =
  print calc

calc =
  let
    risk = calcRisks
    totalRisk = sum (map sum risk)

    trueErosionAtTarget = calcEro 0
    trueRiskAtTarget = calcRisk trueErosionAtTarget
    currentRiskAtTarget = (risk !! targetY) !! targetX
  in
    totalRisk - currentRiskAtTarget + trueRiskAtTarget
    --risk

calcRisks :: [[Int]]
calcRisks =
  let
    firstRow = calcFirstRow
    erosion = calcRows firstRow [firstRow] 1
  in
    map (map calcRisk) erosion

calcFirstRow =
  let
    geo = map ((*) 16807) [0..targetX]
  in
    map calcEro geo

calcRows (_: prevRow) acc y
  | y > targetY = acc
  | otherwise =
      let
        firstCell = calcEro (y * 48271)
        row = calcCells prevRow firstCell [firstCell]
      in
        calcRows row (acc ++ [row]) (y + 1)

calcCells [] _ acc = acc
calcCells (up : restPrevRow) left acc =
  let
    geo = up * left
    newCell = calcEro geo
  in
    calcCells restPrevRow newCell (acc ++ [newCell])

calcEro geo =
  mod (geo + depth) 20183

calcRisk ero =
  mod ero 3