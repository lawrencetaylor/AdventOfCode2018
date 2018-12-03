module Day03 where

import Common
import Data.Either(fromRight)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(many,  sepBy, newline, alphaNum, char, digit, many1  )
import Control.Monad

data Box = Box
  { boxId :: Int
  , xStart :: Int
  , yStart :: Int
  , xWidth :: Int
  , yWidth :: Int } deriving Show

-- Parsing

rawData :: IO String
rawData = readData "data\\Day03"

pBox :: Parser Box
pBox = do
  char '#'
  id <- pInt
  char ' ' >> char '@' >> char ' '
  xStart <- pInt
  char ','
  yStart <- pInt
  char ':' >> char ' '
  xWidth <- pInt
  char 'x'
  yWidth <- pInt
  return $ Box id xStart yStart xWidth yWidth

pLines :: Parser [Box]
pLines = sepBy pBox newline

-- Solution

type BoxId = Int
type Claim = (Int, Int)
type Claims = S.Set Claim
type Claimed = M.Map Claim [BoxId]

claimed :: Box -> Claims
claimed s@(Box _ x y xW yW) =  S.fromList
  [(x + i, y + j) | i <-[0..xW -1 ], j <- [0..yW-1] ]

updateClaims :: BoxId -> Maybe [BoxId] -> Maybe [BoxId]
updateClaims bId Nothing = Just [bId]
updateClaims bId (Just a) = Just $ (bId:a)

addPoint :: BoxId -> Claim -> Claimed -> Claimed
addPoint a p m = M.alter (updateClaims a) p m

addPoints :: Box -> Claimed -> Claimed
addPoints b m = L.foldr (addPoint $ boxId b) m (claimed b)
   
claim :: [Box] -> Claimed
claim boxes = L.foldr addPoints M.empty boxes

claimsCoveredTwice :: [Box] -> Claimed
claimsCoveredTwice = M.filterWithKey (\_ s -> length s >= 2) . claim

boxes :: Claimed -> S.Set BoxId
boxes = S.fromList . concatMap id . M.elems

partOne :: [Box] -> Int
partOne = length . claimsCoveredTwice

partTwo :: [Box] -> BoxId
partTwo b = head $ S.toList $ S.difference (boxes . claim $ b) (boxes . claimsCoveredTwice $ b)

main :: IO ()
main  = do
  input <- (fromRight [] . parse pLines <$> rawData)

  let p1 = partOne input
  putStrLn $ "Day 02 [Part 1] = " ++ (show p1) 

  let p2 = partTwo input
  putStrLn $ "Day 02 [Part 2] = " ++ (show p2) 