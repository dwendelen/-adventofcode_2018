import Data.Ord
import Data.Monoid ((<>))
import Data.List (minimumBy, sortBy, delete, intersect)
import Debug.Trace (trace)
import qualified Data.Map.Strict as Map
import Text.Regex.Posix


data Type = Radiation | Bludgeoning | Fire | Slashing | Cold deriving (Eq, Ord, Show)
data Alliance = Immune | Infection deriving (Eq, Ord, Show)

data Group = Group {
  gId :: String,
  alliance :: Alliance,
  initiative :: Int,
  units :: Int,
  hitPoints :: Int,

  attack :: Int,
  attackType :: Type,

  immunities :: [Type],
  weaknesses :: [Type]
} deriving (Eq, Ord, Show)

getId (Group {gId = gId}) = gId
isImmune (Group {alliance = Immune}) = True
isImmune _ = False
isInfection (Group {alliance = Infection}) = True
isInfection _ = False
getInitiative (Group {initiative = initiative }) = initiative
getUnits (Group {units = units}) = units
getHitPoints (Group {hitPoints = hitPoints}) = hitPoints
getAttackType (Group {attackType = attackType}) = attackType


subtractUnits unitsToSubtract group =
  group { units = (getUnits group) - unitsToSubtract }

getEffectivePower (Group {units = units, attack = attack}) =
  units * attack

isEnemy (Group {alliance = a1}) (Group {alliance = a2}) =
  a1 /= a2
isFriendly (Group {alliance = a1}) (Group {alliance = a2}) =
  a1 == a2

isImmuneTo attackType (Group {immunities = immunities} ) =
  elem attackType immunities
isWeakAgainst attackType (Group {weaknesses = weaknesses }) =
  elem attackType weaknesses

applyBoost boost group
  | isImmune group = group {attack = (attack group) + boost}
  | otherwise = group

calc groups =
  let
    (boost, winning) = try groups 111
    result = sum (map getUnits winning)
  in
    (boost, result)

try group boost | trace ("Try " ++ show boost) False = undefined
try group boost =
  let
    boosted = map (applyBoost boost) group
    sim = simulation [] (Just boosted)
  in
    case sim of
      Nothing -> try group (boost + 1)
      Just res ->
        if isImmune (head res)
          then (boost, res)
          else try group (boost + 1)

simulation prev Nothing = Nothing
simulation prev (Just groups)
  | prev == groups = Nothing
  | winningCondition groups = Just groups
  | otherwise = simulation groups (oneFight groups)

winningCondition groups =
  all isImmune groups || all isInfection groups

oneFight groups =
  let
    selectedTargets = selectTargets groups
  in
    if selectedTargets == []
      then Nothing
      else Just (attackPhase groups selectedTargets)

-- Target selection
selectTargets groups =
  let
    attackers = sortBy choosingOrder groups
    defenders = groups
  in
    selectTargets_ attackers defenders []

choosingOrder a b =
  if getEffectivePower a /= getEffectivePower b
    then compare (getEffectivePower b) (getEffectivePower a)
    else compare (getInitiative b) (getInitiative a)
  -- (flip (comparing getEffectivePower)) <>  (flip (comparing getInitiative))

selectTargets_ :: [Group] -> [Group] -> [(String, String)] -> [(String, String)]
selectTargets_ [] _ acc = acc
selectTargets_ (a: as) defenders acc =
  let
    enemies = filter (isEnemy a) defenders
    defender = selectTarget a enemies
    newDefs = delete defender defenders
    newAcc = (getId a, getId defender) : acc
  in
    if length enemies == 0 || calcDamage a defender == 0
      then selectTargets_ as defenders acc
      else selectTargets_ as newDefs newAcc

selectTarget :: Group -> [Group] -> Group
selectTarget attacker defenders =
  head (sortBy (targetOrder attacker) defenders)

targetOrder :: Group -> Group -> Group -> Ordering
targetOrder attacker a b =
  if calcDamage attacker a /= calcDamage attacker b
    then compare (calcDamage attacker b) (calcDamage attacker a)
    else if getEffectivePower a /= getEffectivePower b
      then compare (getEffectivePower b) (getEffectivePower a)
      else compare (getInitiative b) (getInitiative a)
  --(flip (comparing (calcDamage attacker))) <> ((flip (comparing getEffectivePower)) <> (flip (comparing getInitiative)))

calcDamage attacker defender =
  let
    attackType = getAttackType attacker
    power = getEffectivePower attacker
  in
    if isImmuneTo attackType defender
      then 0
      else
        if isWeakAgainst attackType defender
          then 2 * power
          else power

-- Attacking
attackPhase :: [Group] -> [(String, String)] -> [Group]
attackPhase groups selectedTargets =
  let
    groupsAsMap = listToMap groups
    attacks = sortBy (attackOrder groupsAsMap) selectedTargets
    afterAttacks = foldl executeAttack groupsAsMap attacks
  in
    map snd (Map.toList afterAttacks)

listToMap groups =
  Map.fromList (map (\g -> (getId g, g)) groups)

attackOrder groups (a, _) (b, _) =
  -- (flip (comparing getInitiative)) (groups Map.! a) (groups Map.! b)
  compare (getInitiative (groups Map.! b)) (getInitiative (groups Map.! a))

executeAttack :: Map.Map String Group -> (String, String) -> Map.Map String Group
executeAttack groups (aId, dId)
  | Map.notMember aId groups = groups
  | otherwise =
      let
        attacker = groups Map.! aId
        defender = groups Map.! dId
        groupsWithoutDefender = Map.delete dId groups
        newDefender = takeDamage attacker defender
        groupsWithNewDefender = Map.insert dId newDefender groupsWithoutDefender
      in
        if getUnits newDefender <= 0
          then groupsWithoutDefender
          else groupsWithNewDefender

takeDamage attacker defender =
  let
    damage = calcDamage attacker defender
    nbKills = div damage (getHitPoints defender)
  in
    subtractUnits nbKills defender


examples = [
    Group {
      gId = "Immune 1",
      alliance = Immune,
      units = 17,
      hitPoints = 5390,
      immunities = [],
      weaknesses = [Radiation, Bludgeoning],
      attack = 4507,
      attackType = Fire,
      initiative = 2
    },
    Group {
      gId = "Immune 2",
      alliance = Immune,
      units = 989,
      hitPoints = 1274,
      immunities = [Fire],
      weaknesses = [Bludgeoning, Slashing],
      attack = 25,
      attackType = Slashing,
      initiative = 3
    },
    Group {
      gId = "Infect 1",
      alliance = Infection,
      units = 801,
      hitPoints = 4706,
      immunities = [],
      weaknesses = [Radiation],
      attack = 116,
      attackType = Bludgeoning,
      initiative = 1
    },
    Group {
      gId = "Infect 2",
      alliance = Infection,
      units = 4485,
      hitPoints = 2961,
      immunities = [Radiation],
      weaknesses = [Fire, Cold],
      attack = 12,
      attackType = Slashing,
      initiative = 4
    }
  ]

main = do
    fileImm <- readFile "input_immune.txt";
    fileInf <- readFile "input_infection.txt";
    let
      immLines = lines fileImm
      infLines = lines fileInf

      (_, immunes) = foldl (parseGroup Immune) (1, []) immLines
      (_, infection) = foldl (parseGroup Infection) (1, []) infLines

      groups = (reverse immunes) ++ (reverse infection)
    --print (calc examples)
    print (calc groups)



parseGroup :: Alliance -> (Int, [Group]) -> String -> (Int, [Group])
parseGroup alliance (i, acc) line =
  let
    (_,_,_, match) = line =~ "([0-9]+) units each with ([0-9]+) hit points (\\([^\\)]*\\) )?with an attack that does ([0-9]+) ([a-z]+) damage at initiative ([0-9]+)" :: (String, String, String, [String])
    units = read (match!!0) :: Int
    hitPoints = read (match!!1) :: Int
    (weaknesses, immunities) = parseWeakAndImmu (match!!2)
    attack = read (match!!3) :: Int
    attackType = parseType (match!!4)
    initiative = read (match!!5) :: Int

    group =
      Group {
        gId = show alliance ++ " " ++ show i,
        alliance = alliance,
        units = units,
        hitPoints = hitPoints,
        immunities = immunities,
        weaknesses = weaknesses,
        attack = attack,
        attackType = attackType,
        initiative = initiative
      }
  in
    (i + 1, group : acc)

parseType :: String -> Type
parseType "radiation" = Radiation
parseType "bludgeoning" = Bludgeoning
parseType "fire" = Fire
parseType "slashing" = Slashing
parseType "cold" = Cold

parseWeakAndImmu :: String -> ([Type], [Type])
parseWeakAndImmu line =
  let
    (_, _,_, weaks) = line =~ "weak to ([^;\\)]*)" :: (String, String, String, [String])
    (_, _,_, immunes) = line =~ "immune to ([^;\\)]*)" :: (String, String, String, [String])
    wList = if length weaks == 1 then parseList (weaks !! 0) else []
    iList = if length immunes == 1 then parseList (immunes !! 0) else []
  in
    (wList, iList)

parseList line =
  let
    (_, _,tail, match) = line =~ "([a-z]*), |([a-z]*)" :: (String, String, String, [String])
  in
    if match!!0 == ""
      then [parseType (match!!1)]
      else (parseType (match!!0): parseList tail)