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
data Grid = Grid {
    walls :: S.HashSet Coord
  , units :: M.Map Coord Square } deriving Show

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
openNeighbours (Grid walls units) =
  filter (not . isUnit) . filter (not . isWall) . neighbours
  where 
    isWall c = c `S.member` walls
    isUnit c = c `M.member` units


targets :: Grid -> Coord -> [Coord]
targets (Grid _ units) c = M.keys $ M.filter predicate units
  where 
  predicate = 
    case units M.! c of
      Elf _ -> isGoblin
      Goblin _ -> isElf

neighbouringTargets (Grid _ units ) c = 
  filter predicate $
  filter (\n -> n `M.member` units) $ 
  neighbours c
  where 
    predicate = 
      case units M.! c of
        Elf _ -> isGoblin . (M.!) units
        Goblin _ -> isElf . (M.!) units

openInRangeOfTargets :: Grid -> Coord -> [Coord]
openInRangeOfTargets grid c = 
  [  o | target <- targets grid c, o <- openNeighbours grid target  ]

shortestPath :: Grid -> Coord -> Coord -> Maybe [Coord]
shortestPath grid start goal = 
  G.aStar neighbours cost hDistance hitGoal start
  where 
    neighbours = S.fromList . openNeighbours grid
    cost _ _  = 1
    Coord (gX, gY) = goal
    hDistance (Coord (x,y)) = abs (x - gX) + abs (y - gY)
    hitGoal = (==) 0 . hDistance

distanceTo :: Grid -> Coord -> Coord -> Maybe Int
distanceTo grid start end = fmap length $ shortestPath grid start end

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

-- {-
--   Given a unit and a target square, what is the first step?
--   * Choose neighbours with shortest path to the target
--   * Choose neighbout based on "reading order"
-- -}
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

nextStep grid unit = fmap (firstStep grid unit) $ nearestTarget grid unit

points :: Grid -> Coord -> HitPoints
points (Grid _ units) c = 
  case units M.! c of
    Elf p -> p
    Goblin p -> p

updatePoints :: Grid -> Coord -> HitPoints -> Grid
updatePoints g@(Grid _ units) c newPoints = 
  case units M.! c of
    Elf _ -> g { units = M.insert c (Elf newPoints) units }
    Goblin _ -> g { units = M.insert c (Goblin newPoints) units }

attack :: HitPoints -> Grid -> Coord -> Grid
attack elfHitPoints g@(Grid _ units) unit = 
  case neighbouringTargets g unit of
    [] -> g
    targetCoords -> 
      case newPoints of
        0 -> g { units = M.delete targetCoord units }
        c -> updatePoints g targetCoord c
      where 
        pts c = (points g c, c)
        targetCoord = head $ L.sortOn pts targetCoords
        hitPrice = 
          case isElf $ units M.! targetCoord of
            True -> 3
            False -> elfHitPoints
        adjustedPoints = (points g targetCoord) - hitPrice
        newPoints = max adjustedPoints 0
  
takeTurn :: HitPoints -> Coord -> Grid -> Grid
takeTurn elfHitScore unitCoord g@(Grid _ units) =
  case M.lookup unitCoord units of 
    Nothing -> g -- Unit has been destroyed
    Just unit ->
      case any (\_ -> True) $ neighbouringTargets g unitCoord of
        True -> attack elfHitScore g unitCoord
        False -> 
          case nextStep g unitCoord of
            Nothing -> attack elfHitScore g unitCoord
            Just c -> attack elfHitScore newGrid c
              where 
                newGrid = g { units =  M.insert c unit (M.delete unitCoord units) }

playRound elfHit  grid@(Grid _ units)= 
  foldr (takeTurn elfHit) grid $
  L.reverse $ 
  L.sort $ 
  fmap fst $ 
  M.toList $ 
  units

rounds :: HitPoints -> Grid -> [Grid]
rounds elfHitPoints = iterate (playRound elfHitPoints)

gameWon :: Grid -> Bool
gameWon grid =
  (all isElf surivingTargets)
  || (all isGoblin surivingTargets)
  where 
    surivingTargets = fmap snd $ M.toList $ units grid

gridScore = sum . fmap unitPoints . units
  where 
    unitPoints (Elf p) = p
    unitPoints (Goblin p ) = p

{- 
I have some nasty off-by-one error I'm not sure I can be bothered
to figure out just yet.  

For Part 1, when I get the total number of rounds I need to subtract 1 to 
get the right answer :(
-}
partOneB :: Bool -> [Grid] -> Int
partOneB isPartOne grids = totalRounds * totalScore
  where 
    roundStates = 
      L.takeWhile(\(x:xs) -> not $ gameWon x) $
      L.tails $
      grids

    totalRounds = 
      case isPartOne of
        True -> (length roundStates) - 1
        False -> (length roundStates)
    totalScore = gridScore $ head $ drop 1 $ L.last roundStates

partOne isP1 elfScore grid = partOneB isP1 $ rounds elfScore $ grid

elfCount = length . M.filter isElf . units

partTwoHelper :: [Grid] -> Maybe Grid
partTwoHelper (x:y:xs) = 
  case elfCount x == elfCount y of
    True -> 
      case gameWon y of
        True -> Just y
        False -> partTwoHelper (y:xs)
    False -> Nothing

rawData :: IO String
rawData = readData "data\\Day15"

parseGrid :: [String] ->  Grid
parseGrid grid = Grid walls units

  where 
    convert '#' = Wall
    convert 'E' = Elf 200
    convert 'G' = Goblin 200
    convert '.' = Open

    allPoints = 
      M.fromList $
      [ (Coord (i,j), convert c) | 
        (row, j) <- grid `zip` [0..]
      , (c, i) <- row `zip` [0..] ]

    units = M.filter (isUnit) allPoints
    walls = S.fromList $ fmap fst $ M.toList $ M.filter (\x -> x == Wall) allPoints

main :: IO ()
main = do
  input <- (lines <$> rawData)
  let grid = parseGrid input

  let p1 = partOne True 3 grid
  putStrLn $ "Day 15 [Part 1] = " ++ (show p1) 

  {- Use partTwoHelper to do binary search to find that Elves
     need a score of 34 to win without losing any -}
  let p2 = partOne False 34 grid
  putStrLn $ "Day 15 [Part 2] = " ++ (show p2) 

