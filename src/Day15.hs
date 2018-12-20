module Day15 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(sepBy, newline, string) 
import qualified Data.Maybe as M
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.Graph.AStar as G
import qualified Data.HashSet as S
import qualified Data.Hashable as H
import qualified Safe as Safe

{-
In each round:
 * Each alive unit take a turn 
   * Unit tries to move into range of an enemy
   * Then attacks if in range


Move: never diagonally
units take turns in reading order

If not in range of target
1) Identify all targets
2) Identify all open squares in range of each target
In range == up/down/left/right of target not occupied
IF thera are no available open squares in range of a target - unit ends turn
IF unit is already in range it does to move - it attacks

Move:
Considers all the open square in range, and works out which
one it could reach in fewest number of steps (u/d/l/r) - 
using current position of units.  If it cannot find a path  - doesn't move.
For tied squares - first reading order wins.
  
-}

type HitPoints = Int
data Square = Elf HitPoints | Goblin HitPoints | Wall | Open deriving (Show, Ord, Eq)
newtype Coord = Coord (Int, Int) deriving (Eq, Show)
type Grid = M.Map Coord Square

-- Reading Order
instance Ord Coord where
  (Coord (x1, y1)) `compare` (Coord (x2,y2)) = 
    case y1 == y2 of
      True -> compare x1 x2
      False -> compare y1 y2

instance H.Hashable Coord where 
  hashWithSalt salt (Coord (x, y)) = H.hashWithSalt salt (x,y)


xVal :: Coord -> Int
xVal (Coord (x, _)) = x

yVal :: Coord -> Int
yVal (Coord (_, y)) = y

isOpen Open = True
isOpen _    = False

isElf (Elf _) = True
isElf _          = False

isGoblin (Goblin _) = True
isGoblin _             = False

isUnit (Goblin _) = True
isUnit (Elf _) = True
isUnit _        = False

neighbours :: Coord -> [Coord]
neighbours (Coord (x,y)) = 
  fmap Coord $
  [ (x + 1, y)
  , (x - 1, y)
  , (x, y + 1)
  , (x, y - 1)  ]

openNeighbours :: Grid -> Coord -> [Coord]
openNeighbours grid = 
  filter (isOpen . (M.!) grid) . neighbours
  
units :: Grid -> [Coord]
units = fmap fst . M.toList . M.filter isUnit

targets :: Grid -> Coord -> [Coord]
targets grid c = M.keys $ M.filter predicate grid
  where 
  predicate = 
    case grid M.! c of
      Elf _ -> isGoblin
      Goblin _ -> isElf
      otherwise -> error "Expected Elf or Goblin"

-- isInRangeOfTarget :: Grid -> Coord -> Bool
neighbouringTargets grid c = filter predicate $ neighbours c
  where 
    predicate = 
      case grid M.! c of
        Elf _ -> isGoblin . (M.!) grid
        Goblin _ -> isElf . (M.!) grid
        c -> error $ "Expected Elf or Goblin: " ++ (show c)

openInRangeOfTargets :: Grid -> Coord -> [Coord]
openInRangeOfTargets grid c = 
  [  o | target <- targets grid c, o <- openNeighbours grid target  ]

paths :: Grid -> Coord -> Coord -> Maybe [Coord]
paths  = undefined

shortestPath :: Grid -> Coord -> Coord -> Maybe [Coord]
shortestPath grid start goal = 
  G.aStar neighbours cost hDistance hitGoal start
  where 
    neighbours = S.fromList . openNeighbours grid
    cost _ _  = 1 -- Cost currently give equal weighting to both paths
    Coord (gX, gY) = goal
    hDistance (Coord (x,y)) = abs (x - gX) + abs (y - gY)
    hitGoal = (==) 0 . hDistance

{-
1) Find targets that are closest to unit
2) For each target find neightbour with shortest path to that target
3) Take unit (tie break reading order) with shortest path
-}

tuple :: a -> (a,a)
tuple x = (x,x)

pathLengthOrder :: (Ord a) => (b, Maybe a) -> (b, Maybe a) -> Ordering
pathLengthOrder (_, (Just a)) (_,  (Just b)) = compare a b
pathLengthOrder (_, (Just a)) (_,  Nothing) = LT
pathLengthOrder (_, Nothing ) (_, (Just b)) = GT
pathLengthOrder (_, Nothing ) (_, Nothing )= EQ

distanceTo :: Grid -> Coord -> Coord -> Maybe Int
distanceTo grid start end = fmap length $ shortestPath grid start end

-- targetsClosest :: Grid -> Coord -> [Coord]
nearestTarget grid unit = 
  Safe.headMay $
  L.sort $
  fmap fst $
  M.catMaybes $
  traverse id $
  Safe.headMay $
  L.groupOn snd $
  L.sortOn snd $
  M.catMaybes $
  fmap (traverse id) $
  fmap (\t -> (t, distanceTo grid unit t)) $
  openInRangeOfTargets grid unit

{-
  Given a unit and a target square, what is the first step?
  * Choose neighbours with shortest path to the target
  * Choose neighbout based on "reading order"
-}
firstStep grid unit target =
  head $
  (L.sort) $
  fmap fst $
  M.catMaybes $
  traverse id $
  Safe.headMay $
  L.groupOn snd $
  L.sortOn snd $
  filter (M.isJust . snd) $
  fmap (\t -> (t, distanceTo grid t target )) $
  openNeighbours grid unit

-- nextStep :: Grid -> Coord -> Maybe Coord
nextStep grid unit = fmap (firstStep grid unit) $ nearestTarget grid unit

points grid c = 
  case grid M.! c of
    Elf p -> p
    Goblin p -> p
    otherwise -> error "No points for Non-Unit"

updatePoints grid c newPoints = 
  case grid M.! c of
    Elf _ -> M.insert c (Elf newPoints) grid
    Goblin _ -> M.insert c (Goblin newPoints) grid

attack grid unit = 
  case neighbouringTargets grid unit of
    [] -> grid
    targetCoords -> 
      case newPoints of
        0 -> M.insert (debug "Destroyed" targetCoord) Open grid
        c -> updatePoints grid (debug "Hurt" targetCoord) c
      where 
        pts c = (points grid c, c)
        targetCoord = head $ L.sortOn pts targetCoords
        adjustedPoints = (points grid targetCoord) - 3
        newPoints = debug "New Points" $ max adjustedPoints 0
        
takeTurn unitCoord grid =
  case isUnit (grid M.! unitCoord) of 
    False -> grid -- Unit has been destroyed
    True ->
      case any (\_ -> True) $ neighbouringTargets grid (debug "=== Turn" unitCoord) of
        True -> attack grid $ debug "Attacker1:" unitCoord
        False -> 
          case nextStep grid unitCoord of
            Nothing -> attack grid $ debug "Attacker2:" unitCoord
            Just c -> attack newGrid (debug (show unitCoord ++ " attacked and moved in to ") c)
              where 
                unit = grid M.! unitCoord
                newGrid = M.insert c unit (M.insert unitCoord Open grid)

testRound grid n = foldr takeTurn grid $ L.reverse $ take n $ L.sort $ units grid

playRound grid = foldr takeTurn grid $ L.reverse $ L.sort $ units grid

rounds :: Grid -> [Grid]
rounds = iterate playRound

gameWon :: Grid -> Bool
gameWon grid =
  (all isElf surivingTargets)
  || (all isGoblin surivingTargets)
  where 
    surivingTargets = M.filter isUnit grid

partOneB :: [Grid] -> Int
partOneB grids = scoreTotal  * totalRounds
  where 
    roundStates = 
      L.takeWhile(\(x:xs) -> not $ gameWon x) $
      L.tails $
      grids

    totalRounds = (length roundStates) - 1

    finalGrid = head $ drop 1 $ L.last roundStates
    scoreTotal = 
      sum $ 
      fmap (points finalGrid) $
      M.keys $ 
      M.filter isUnit finalGrid


partOne :: Int -> [Grid] -> Int
partOne round (x:xs) = 
  case gameWon x of
    False -> partOne (round + 1) xs
    True -> scoreTotal *(debug "Round" (round - 1))
      where 
        scoreTotal = sum $ fmap (points x) $ M.keys $ M.filter isUnit x

testRounds grid n m = testRound ((iterate playRound grid ) !! n) m
    
rawData :: IO String
rawData = readData "data\\Day15"

partTwo :: String -> String
partTwo = id


test :: [String]
test = 
  [ "#######"
  , "#E..G.#"
  , "#...#.#"
  , "#.G.#G#"
  , "#######" ]

test2 :: [String]
test2 = 
  [ "#######"
  , "#.E...#"   
  , "#.....#"
  , "#...G.#"
  , "#######"]

test3 :: [String]
test3 = 
  [ "#########"
  , "#G..G..G#"
  , "#.......#"
  , "#.......#"
  , "#G..E..G#"
  , "#.......#"
  , "#.......#"
  , "#G..G..G#"
  , "#########" ]

test3a :: [String]
test3a = 
  [ "#########"
  , "#..G.G..#"
  , "#...G...#"
  , "#.G.E.G.#"
  , "#.......#"
  , "#G..G..G#"
  , "#.......#"
  , "#.......#"
  , "#########"]

test4 :: [String]
test4 = 
  [ "#######"   
  , "#.G...#" 
  , "#...EG#" 
  , "#.#.#G#" 
  , "#..G#E#" 
  , "#.....#"
  , "#######" ]


test6 :: [String]
test6 = 
  [ "#######"
  , "#G..#E#"
  , "#E#E.E#"
  , "#G.##.#"
  , "#...#E#"
  , "#...E.#"
  , "#######"]

test7 = 
  [ "#######"
  , "#E..EG#"
  , "#.#G.E#"
  , "#E.##E#"
  , "#G..#.#"
  , "#..E#.#"
  , "#######" ]
-- 9 rounds : Elf has 140
test20 = 
  [ "#########"
  , "#G......#"
  , "#.E.#...#"
  , "#..##..G#"
  , "#...##..#"
  , "#...#...#"
  , "#.G...G.#"
  , "#.....G.#"
  , "#########"
  ]

test54 = 
  [ "#######"
  , "#.E...#"
  , "#.#..G#"
  , "#.###.#"
  , "#E#G#G#"
  , "#...#G#"
  , "#######"]

test35 = 
  [ "#######"
  , "#E.G#.#"
  , "#.#G..#"
  , "#G.#.G#"
  , "#G..#.#"
  , "#...E.#"
  , "#######" ]

testGrid :: [String] ->  Grid
testGrid grid = 
  M.fromList $
  [ (Coord (i,j), convert c) | 
    (row, j) <- grid `zip` [0..]
  , (c, i) <- row `zip` [0..] ]
  where 
    convert '#' = Wall
    convert 'E' = Elf 200
    convert 'G' = Goblin 200
    convert '.' = Open

display :: Grid -> String
display grid = gridString ++ "\n" ++ (show scores)
  where 
    convert Wall = '#'
    convert (Elf _) = 'E'
    convert (Goblin _) = 'G'
    convert Open = '.'

    gridString = 
      concatMap id $
      L.intersperse "\n" $
      fmap (fmap (convert . snd )) $
      L.groupOn (yVal . fst) $
      M.toList grid

    scores = L.sort $ (snd <$> (M.toList $ M.filter isUnit grid))

main :: IO ()
main = do
  input <- (lines <$> rawData)

  let p1 = partOneB $ rounds $ testGrid input
  putStrLn $ "Day 15 [Part 1] = " ++ (show p1) 

  -- let p2 = partTwo input
  -- putStrLn $ "Day 15 [Part 2] = " ++ (show p2) 

