{-# Language TupleSections #-}

module Day17 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(sepBy, newline, string, (<|>)) 
import qualified Data.Set as S
import qualified Data.Tree as T
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.Extra as L

type Location = (Int, Int)
type Seam = [Location]
type Clay = S.Set Location

pHorizontalSeam :: Parser Seam
pHorizontalSeam = do
  x <- string "x=" *> pInt
  yStart <- string ", y=" *> pInt
  yEnd <- string ".." *> pInt
  return [(x, y) | y <- [yStart..yEnd]]

pVerticalSeam :: Parser Seam
pVerticalSeam = do
  y <- string "y=" *> pInt
  xStart <- string ", x=" *> pInt
  xEnd <- string ".." *> pInt
  return [(x, y) | x <- [xStart..xEnd]]

pSeam :: Parser Seam
pSeam = pHorizontalSeam <|> pVerticalSeam

pFile :: Parser Clay
pFile = S.fromList . concatMap id <$> sepBy pSeam newline

-- 

{-
......+.......    (line not counted: above minimum y value)
......|.....#.
.#..#||||...#.
.#..#~~#|.....
.#..#~~#|.....
.#~~~~~#|.....
.#~~~~~#|.....
.#######|.....
........|.....
...|||||||||..
...|#~~~~~#|..
...|#~~~~~#|..
...|#~~~~~#|..
...|#######|..
...|.......|..    (line not counted: below maximum y value)
...|.......|..    (line not counted: below maximum y value)
...|.......|..    (line not counted: below maximum y value)

Each new member of flow goes on to stack

If we hit clay going (L, DOWN)
  Put (right L, and left L) on stack 

Start with one flow
Put on to "stack"
Pull off stack -
  If we are going Right and Hit Clay 

  If we are going Left and Hit Clay - add to water
"Split"
Put these on the stack with direction

-}

data Sand = Wet | Clay | Water deriving (Show)
type Ground = M.Map Location Sand 
data FlowDirection = L | R | D | U deriving (Ord, Eq, Show)
type IsFork = Bool
type LocationState = (Location, FlowDirection, IsFork)
type Flow = [LocationState]
type YLimit = Int

{-
(500,0) D
(500,1) D
..
(500,6) D
(500,6) R
(500,6) L

Pop (500,6) L - (501,6) is Clay - Backtrack to when direction is R - adding no water
Pop (500,6) R - 

(499, 6) L
..
(496, 6) L - (495, 5) is Clay - Backtrack to when direction is D - adding water
-}

maxY :: Ground -> Int
maxY = maximum . fmap snd . fmap fst . M.toList

yCoord :: Location -> Int
yCoord (_, y) = y

isClay :: Ground -> Location -> Bool
isClay ground l = 
  case M.lookup l ground of
    Just Clay -> True
    _ -> False

isWater :: Ground -> Location -> Bool
isWater ground l = 
  case M.lookup l ground of 
    Just Water -> True
    _ -> False

isWet :: Ground -> Location -> Bool
isWet ground l = 
  case M.lookup l ground of 
    Just Wet -> True
    _ -> False

try :: Int -> IO ()
try n = do
  let yLimit = maxY testSeams
  let (g, f) = (iterate $ flow yLimit) (testSeams , initial ) !! n
  putStrLn $ display g
  putStrLn $ show f 
  putStrLn $ "Max Y: " ++ (show $ yLimit)

initial :: Flow
initial = [((500, 0), D, False)]

backTrack :: Flow -> ([Location], Flow)
backTrack f = bT [] f
    where 
      bT locs flow@((l, _, isFork):xf)
        | isFork = (l : locs, xf)
        | otherwise = bT (l : locs) xf

fill :: Ground -> Location -> Ground
fill ground l = foldl (\m a -> M.insert a Water m) ground standingWater
  where 
    standingWaterL = takeWhile (not . isClay ground) (iterate left l)
    standingWaterR = takeWhile (not . isClay ground) (iterate right l)
    standingWater = standingWaterL ++ standingWaterR


flow :: YLimit -> (Ground, Flow) -> (Ground, Flow)
flow yLimit (ground, f@((l, D, _):xs)) 
    | yCoord l > yLimit = (newGround, newFlow)
    | (isClay ground) (below l) || (isWater ground ) (below l) = (ground, (l, L, True) : (l, R, True) : tail f)
    | (isWet ground) (below l) = (fill ground $ below l, f)
    | (not . isClay ground ) (below l) = (M.insert (below l) Wet ground, (below l , D, False) : f)
    | (isWater ground) l = (ground, tail f)
    where 
      (newWater, newFlow) = backTrack f 
      newGround = foldl (\m a -> M.insert a Wet m) ground newWater

flow _ (ground, f@((l, L, _):xs))
    | (not . isWater ground) (below l) && (not . isClay ground) (below l) = 
      (M.insert (below l) Wet ground, (below l, D, False) : f )
    | (not . isClay ground) (left l) = (M.insert (left l) Wet ground, (left l, L, False) : f)
    | (isClay ground) (left l) = (newGround, newFlow)     
     where 
        (newWater, newFlow) = backTrack f 

        newGround = foldl (\m a -> M.insert a Wet m) ground newWater

flow  _ (ground, f@((l, R, _):xs))
    | (not . isWater ground) (below l) && (not . isClay ground) (below l) = 
      (M.insert (below l) Wet ground, (below l, D, False) : f )
    | (not . isClay ground) (right l) = (M.insert (right l) Wet ground, (right l, R, False) : f)
    | (isClay ground) (right l) = (newGround, newFlow)
      where 
        (newWater, newFlow) = backTrack f 
        newGround = foldl (\m a -> M.insert a Wet m) ground newWater


next n (x,y) = 
  filter (\(x,y) -> y < n)
    $ filter (\(x,y) -> x < n ) 
    $ [(x,y+1), (x+1,y+1), (x+1,y)]



below :: Location -> Location
below (x,y) = (x, y+1)

above :: Location -> Location
above (x,y) = (x, y-1)

right :: Location -> Location
right (x, y) = (x+1, y)

left :: Location  -> Location
left (x,y) = (x-1,y)








{-
|
|
|
|
~~~~~~~~#
~~~~~~~~#
-}

-- flow water clay (l, D) 
--   | isFilled (below l) =
--     filter (\(loc, _) -> not $ S.member loc clay )
--       $ [onRightS l, onLeftS l, aboveS l]
--   where 
--     isFilled l = S.member l clay || S.member l water
  



connectedTo :: (LocationState -> [LocationState]) -> LocationState -> S.Set LocationState
connectedTo newLocations seedLocation = buildComponent S.empty [seedLocation]
  where 
    buildComponent seen [] = seen
    buildComponent seen (x:xs) =
      buildComponent (S.insert x seen) 
        $ filter (\x -> not $ S.member x seen)
        $ (newLocations x) ++ xs

type Water = S.Set Location







x :: Clay -> Water -> Location -> [Location]
x clay water (x, y) = undefined
  where 
    isClay l = l `S.member` clay
    isWater l = l `S.member` water

    clayBelow = isClay (x, y + 1)
    waterBelow = isClay (x, y + 1)

    x2 = 
      filter (not . isWater) $
      filter (not . isClay) $ 
        [(x+1, y), (x-1, y)]


fillRow :: Clay -> Location -> (Water, Location)
fillRow clay (x, y) = (S.fromList row, (x, y-1))
  where 
    isClay l = l `S.member` clay

    rowToRight = 
      takeWhile (not . isClay) $
        [ (x+i, y) | i <- [1..] ]
    rowToLeft = 
      takeWhile (not . isClay) $
        [ (x-i, y) | i <- [1..] ]

    row = rowToLeft ++ [(x,y)] ++ rowToLeft

--

testData = 
  [ "x=495, y=2..7"
  , "y=7, x=495..501"
  , "x=501, y=3..7"
  , "x=498, y=2..4"
  , "x=506, y=1..2"
  , "x=498, y=10..13"
  , "x=504, y=10..13"
  , "y=13, x=498..504" ]

testSeams = 
  M.fromList
    $ fmap (, Clay) 
    $ concatMap id
    $ fromRight undefined
    <$> parse pSeam
    <$> testData

display ground =
  concatMap id 
    $ L.intersperse "\n" 
    $ row 
    <$> [0..maxY]
  where 
    list = M.toList ground
    minX = minimum $ (fst . fst) <$> list
    maxX = maximum $ (fst . fst) <$> list
    maxY = maximum $ (snd . fst) <$> list
    toChar (500, 0) = '+'
    toChar l = 
      case M.lookup l ground of
        Nothing -> '.'
        Just Wet -> '|'
        Just Clay -> '#'
        Just Water -> '~'
    row y = [ toChar (x, y) | x <- [minX..maxX] ]

    

rawData :: IO String
rawData = readData "data\\Day17-sample"

partOne :: String -> String
partOne = id

partTwo :: String -> String
partTwo = id

main :: IO ()
main = do
  -- input <- (fromRight [] . parse pPoints <$> rawData)
  let input = "ABC"

  let p1 = partOne $ input
  putStrLn $ "Day 17 [Part 1] = " ++ (show p1) 

  let p2 = partTwo input
  putStrLn $ "Day 17 [Part 2] = " ++ (show p2) 

