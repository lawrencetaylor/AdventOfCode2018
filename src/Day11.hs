module Day11 where

import Data.Either(fromRight)
import Data.Maybe(maybe)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(sepBy, newline, string) 
import qualified Data.List as L
import qualified Data.Map as M
import Debug.Trace
import qualified Data.Vector.Algorithms.Search as V

-- Find the fuel cell's rack ID, which is its X coordinate plus 10.
-- Begin with a power level of the rack ID times the Y coordinate.
-- Increase the power level by the value of the grid serial number (your puzzle input).
-- Set the power level to itself multiplied by the rack ID.
-- Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
-- Subtract 5 from the power level.

-- Fuel cell at  122,79, grid serial number 57: power level -5.
-- Fuel cell at 217,196, grid serial number 39: power level  0.
-- Fuel cell at 101,153, grid serial number 71: power level  4

-- For grid serial number 18, the largest total 3x3 square has a top-left corner of 33,45
--  (with a total power of 29); these fuel cells appear in the middle of this 5x5 region:

newtype Coord = Coord (Int, Int) deriving (Show, Eq, Ord)

instance Semigroup Coord where
  (Coord (x1, y1)) <> (Coord (x2,y2)) = Coord (x1+x2, y1+y2) 

type Powers = M.Map Coord Int

power :: Int -> Coord -> Int
power serial (Coord (x, y)) = powerLevel2 - 5
  where 
    rackId = x + 10
    powerLevel = ( (rackId * y) + serial ) * rackId
    powerLevel2 = (powerLevel `quot` 100) `rem` 10

-- Create grid of powers from serial number
powers :: Int -> Powers
powers serial = 
  M.fromList $ 
  (\x -> (x, power serial x)) 
  <$> (Coord <$> ((,) <$> [1..300] <*> [1..300]))

-- Coordinates contains within a square with top left-hand
-- corner the specified coordinate
squares :: Int -> Coord -> [Coord]
squares squareSize c = (<>) c <$> (Coord <$> ((,) <$> [0..limit] <*> [0..limit]))
  where 
    limit = squareSize - 1

-- Create a new grid from a given grid whose value at (x,y)
-- is the sum of those values in the original grid of all
-- values [ (i,j) | i <- [1..x], j <- [1..y]]

-- This grid G can be defined recursively in terms of the original grid M as
-- G[1,1] = M[1,1]
-- G[n,1] = G[n-1, 1] + M[n,1]
-- G[1,n] = G[1, n-1] + M[1,n]
-- G[n,m] = G[n-1,m] + G[m,n-1] - G[n-1,m-1]
accumulateSums :: M.Map Coord Int -> M.Map Coord Int -> Coord -> (Int, M.Map Coord Int)
accumulateSums m mSums c@(Coord (1, 1)) = (seed, M.insert c seed mSums)
  where 
    seed = m M.! c
accumulateSums m mSums c@(Coord (1, y)) = 
  case c `M.lookup` mSums of
    Just s -> (s, mSums )
    Nothing -> (thisValue, M.insert c thisValue m1)
      where 
         (value, m1) = accumulateSums m mSums (Coord (1, y - 1))
         thisValue = value + (m M.! c)
accumulateSums m mSums c@(Coord (x, 1)) = 
  case c `M.lookup` mSums of
    Just s -> (s, mSums )
    Nothing -> (thisValue, M.insert c thisValue m1)
      where 
          (value, m1) = accumulateSums m mSums (Coord (x-1, 1))
          thisValue = value + (m M.! c)
accumulateSums m mSums c@(Coord (x, y)) = 
  case c `M.lookup` mSums of
    Just s -> (s, mSums )
    Nothing -> (thisValue, M.insert c thisValue mS3)
      where 
          (value1, mS1) = accumulateSums m mSums (Coord (x-1, y))
          (value2, mS2) = accumulateSums m mS1 (Coord (x, y-1))
          (value3, mS3) = accumulateSums m mS2 (Coord (x-1, y-1))
          thisValue = value1 + value2 - value3 + (m M.! c)

-- Can use grid of "accumulated sums" to calculate 
-- totals of squares:

-- XXXXXXXXXXXXXXXXXXXXXX
-- XXXXaXXXXXXXXbXXXXXXXX
-- XXXXX.........XXXXXXXX
-- XXXXX.........XXXXXXXX
-- XXXXc........dXXXXXXXX
-- XXXXXXXXXXXXXXXXXXXXXX
-- XXXXXXXXXXXXXXXXXXXXXX

-- Total = D - C - B + A (since A has been taken away twice)

squareTotalPower :: Int -> Powers -> Coord -> Int
squareTotalPower squareSize sumPowers (Coord (x, y)) =
  value d + value a - value c - value b
  where
    a = Coord (x - 1, y - 1)
    b = Coord (x + squareSize - 1, y - 1)
    c = Coord (x - 1 , y + squareSize - 1)
    d = Coord (x + squareSize - 1, y + squareSize - 1)

    value k = maybe 0 id $ M.lookup k sumPowers

coordWithLargestPower :: Powers -> Int -> (Coord, Int)
coordWithLargestPower powers squareSize =
  head $
  L.sortOn (negate . snd) $
  (\c -> (c, squareTotalPower squareSize powers c)) <$>
  Coord <$> ((,) <$> [1..limit] <*> [1..limit])
  where 
    limit = 300 - squareSize


partOne powers = (x,y)
  where 
    (Coord (x,y), _) =  coordWithLargestPower powers 3


partTwo powers = (x, y, square)
  where 
    ((Coord (x,y),_), square) = 
      head $ 
      L.sortOn (negate . snd . fst) $ 
      (coordWithLargestPower powers <$> [1..299]) `zip` [1..299]

    
main :: IO ()
main = do
  let coordPowers = powers 3999
  let (_, complexSums) = accumulateSums coordPowers M.empty(Coord (300,300))
  putStrLn $ "Day 11 [Part 1] = " ++ (show $ partOne complexSums)
  putStrLn $ "Day 11 [Part 2] = " ++ (show $ partTwo complexSums)

  
