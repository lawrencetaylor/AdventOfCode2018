module Day10 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError, (<|>))
import Text.Parsec(sepBy, newline, string, char, spaces, many) 
import qualified Data.List as L

-- Parsing

-- position=<-3,  6> velocity=< 2, -1>

pNegInt :: Parser Int
pNegInt = do
  char '-'
  i <- pInt
  return $ (-1)*i

pI :: Parser Int
pI = pNegInt <|> pInt

pPoint :: Parser Point
pPoint = do
  string "position=<" <* spaces
  pX <- pI
  char ',' <* spaces
  pY <- pI
  string "> velocity=<" <* spaces
  vX <- pI
  char ',' <* spaces
  vY <- pI
  char '>'
  return $ Point (pX, pY) (vX,vY)

pPoints :: Parser [Point]
pPoints = sepBy pPoint newline

data Point = Point {
    position :: (Int, Int)
  , velocity :: (Int, Int)
  } deriving Show

move :: Point -> Point
move ( Point (x,y) (vX, vY) ) = 
  Point (x + vX, y + vY) (vX, vY)

tick :: [Point] -> [Point]
tick pts = move <$> pts

bounds :: [Point] -> ((Int, Int), (Int, Int))
bounds pts = ((minX, minY), (maxX, maxY))
  where 
    positions = position <$> pts
    minX = minimum $ fst <$> positions
    minY= minimum $ snd <$> positions
    maxX = maximum $ fst <$> positions
    maxY = maximum $ snd <$> positions

grid :: [Point] -> [[(Int, Int)]]
grid pts = rows
  where 
    positions = position <$> pts
    ((minX, minY), (maxX, maxY)) = bounds pts
    xRange = [ i | i <- [minX..maxX]]
    yRange = [ j | j <- [minY..maxY]]
    rows = (\y -> [(x, y) | x <- xRange])  <$> yRange

display :: [Point] -> String
display pts = y
  where
    points = position <$> pts
    isFilled x = points `elem` x
    g = grid pts
    y = mconcat $ L.intersperse "\n" $ rowToString points <$> g

rowToString :: [(Int, Int)] -> [(Int, Int)] -> String
rowToString points row = toChar <$> row
  where
    toChar coord | (coord `elem` points) = '#'
    toChar _ = '.'

getMessage :: ([Point], Int) -> ([Point], Int)
getMessage (pts, seconds) = 
  case xRange pts <= 18 of -- 18 found by trial and error
  True -> (pts, seconds)
  False -> getMessage ((tick pts), seconds + 1)

xRange :: [Point] -> Int
xRange pts = abs $ yMin - yMax
  where ((_, yMin), (_, yMax)) = bounds pts

rawData :: IO String
rawData = readData "data\\Day10"

main :: IO ()
main = do
  points <- (fromRight [] . parse pPoints <$> rawData)

  let (message, seconds) = getMessage (points, 0)
  let p1 = display $ message
  putStrLn $ "Day 10 [Part 1] = \n" ++ (p1) 

  let p2 = seconds
  putStrLn $ "Day 10 [Part 2] = " ++ (show seconds) 


