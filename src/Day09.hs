module Day09 where

import Data.Either(fromRight)
import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(sepBy, newline, string) 
import qualified Data.Map as M
import Data.Sequence((><), (<|))
import qualified Data.Sequence as S

rawData :: IO String
rawData = readData "data\\Day09"

type Elf = Int
type Elves  = M.Map Elf Int
type Marble = Int
data GameState = GameState {
    marbles :: S.Seq Int
  , elves :: Elves
  , currentElf :: Int
  , marble :: Int
  } deriving Show

addScore :: Int -> Maybe Int -> Maybe Int
addScore a Nothing = Just a
addScore a (Just b) = Just (a + b)

play :: Int -> Int -> GameState -> Int
play _ maxMarble ( GameState { marbles = marbles, marble = marble, elves = elves } )
  | marble > maxMarble = maximum $ snd <$> M.toList elves 

play numberOfElves maxMarble (GameState { currentElf = elf, marble = marble, marbles = marbles, elves = elves})
  | marble `mod` 23 /= 0 = 
    
    -- I have no idea why this works!  When I put the "Split" inside 
    -- the "where" clause I get a stack overflow....

    case S.splitAt newIndex marbles of
      (l,r) -> play
                  numberOfElves
                  maxMarble $
                  GameState (S.singleton marble >< r >< l) elves nextElf (marble + 1)
      where 
        newIndex = 2 `mod` (S.length marbles)
        (before, after) = S.splitAt newIndex marbles
        newMarbles = S.singleton marble >< after >< before
        nextElf = (elf + 1) `mod` numberOfElves

play numberOfElves maxMarble (GameState { currentElf = elf, marble = marble, marbles = marbles, elves = elves}) =
  play numberOfElves maxMarble $ GameState newMarbles newElves nextElf (marble + 1)
  where 
    currentMarbles = marbles 
    numberOfMarbles = length currentMarbles
    indexToRemove = numberOfMarbles - 7
    marbleToRemove = currentMarbles  `S.index` indexToRemove
    newScore = marble + marbleToRemove
    newElves = M.alter (addScore newScore) elf elves
    newMarbles = S.take (numberOfMarbles-1) $ S.drop (indexToRemove + 1) $ (currentMarbles >< currentMarbles)
    nextElf = (elf + 1) `mod` numberOfElves

main :: IO ()
main = do

  let p1 = play 439 71307 (GameState (S.fromList [0]) M.empty 0 1)
  putStrLn $ "Day 09 [Part 1] = " ++ (show p1) 

  let p2 = play 439 7130700 (GameState (S.fromList [0]) M.empty 0 1)
  putStrLn $ "Day 09 [Part 2] = " ++ (show p2) 

