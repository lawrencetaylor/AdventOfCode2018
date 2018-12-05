module Day04 where

import Common
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec(many,  sepBy, newline, alphaNum, char, string, (<|>)  )
import Data.Time
import Data.Either(fromRight)
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map((!))
import Data.Tuple
import Data.Ord

data Event = 
  BeginsShift Int
  | WakesUp
  | FallsAsleep deriving Show

-- Parsing

pDate :: Parser UTCTime
pDate = do
  year <- toInteger <$> pInt
  char '-'
  month <- pInt
  char '-'
  day <- pInt
  char ' '
  hour <- pInt
  char ':'
  minutes <- pInt
  let date = fromGregorian year month day
  let seconds = secondsToDiffTime . toInteger $ minutes*60 + hour*60*60
  return $ UTCTime date seconds

pWakesUp :: Parser Event
pWakesUp = do
  string "wakes up"
  return WakesUp

pFallsAsleep :: Parser Event
pFallsAsleep = do
  string "falls asleep"
  return FallsAsleep

pBeginsShift :: Parser Event
pBeginsShift = do
  string "Guard #"
  guardId <- pInt
  many alphaNum
  return $ BeginsShift guardId

pEvent :: Parser (UTCTime, Event)
pEvent = do
  char '[' 
  time <- pDate
  char ']'
  char ' '
  event <- pWakesUp <|> pFallsAsleep <|> pBeginsShift
  return (time, event)

pEvents :: Parser [(UTCTime, Event)]
pEvents = sepBy pEvent newline

rawData :: IO String
rawData = readData "data\\Day04"

type Guard = Int
type Minute = Int
type NapInterval = (UTCTime, UTCTime)

data State = State 
  { runningTotals ::  M.Map Guard [NapInterval]
  , currentGuard :: Int
  , fellAsleep :: Maybe UTCTime } deriving Show

startingState = State M.empty 0 Nothing

updateNaps :: Guard -> NapInterval -> Maybe [NapInterval] -> Maybe [NapInterval]
updateNaps gId interval Nothing = Just [interval]
updateNaps gId interval (Just i) = Just $ (interval:i)

buildState :: (UTCTime, Event) -> State -> State
buildState (t, BeginsShift gId) s = s { currentGuard = gId, fellAsleep = Nothing }
buildState (t, FallsAsleep)     s = s { fellAsleep = Just t}
buildState (t, WakesUp )        s = s { runningTotals = newTotals, fellAsleep = Nothing }
  where 
    gId = currentGuard s
    (Just sleepTime) = fellAsleep s
    interval = (sleepTime, t)
    currentTotals = runningTotals s
    newTotals = M.alter (updateNaps gId interval) gId currentTotals
    
runState :: [(UTCTime, Event)] -> State
runState events = L.foldr buildState startingState events

getSeconds :: NapInterval -> Int
getSeconds (t1, t2) = truncate . toRational $ diffUTCTime t2 t1

minuteIntervals :: NapInterval -> [UTCTime]
minuteIntervals (t1, t2) = L.unfoldr go t1
  where 
    addMinute t = addUTCTime (secondsToNominalDiffTime 60) t

    go t = 
      case t < t2 of
        True -> Just (t, addMinute t)
        False -> Nothing

mostFrequent :: [Int] -> Int
mostFrequent = 
  head . L.maximumBy (comparing length) . L.group . L.sort

minute :: UTCTime -> Int
minute t = mins
  where 
    TimeOfDay _ mins _ = timeToTimeOfDay (utctDayTime t)

partOne :: State -> Int
partOne s = guardId * sleepiestMin
  where 
    guardSleepTotals = sum <$> (\x -> getSeconds <$> x) <$> runningTotals s
    (guardId, _) = head $ L.sortOn (negate . snd) $ M.toList guardSleepTotals
    sleepiestMin = mostFrequent $ minute <$> (concatMap minuteIntervals $ (runningTotals s) ! guardId)

minuteFreq :: [Int] -> [(Int, Int)]
minuteFreq = map (\x -> (head x, length x)) . L.group . L.sort

partTwo :: State -> Int
partTwo s = guardId * guardsSleepiestMinute
  where 
    (guardId, (guardsSleepiestMinute, _)) = 
      head $
      L.sortOn (negate . snd . snd ) $
      M.toList $
      M.map head $
      M.map (L.sortOn (negate . snd)) $
      M.map minuteFreq $
      M.map ((<$>) minute) $  
      M.map (concatMap id) $ 
      M.map ((<$>) minuteIntervals) $
      runningTotals s

main :: IO ()
main = do
  input <- fromRight [] <$> traverse (parse pEvent) . lines <$> rawData
  let sortedInput = L.sortOn fst input
  let state = runState $ reverse sortedInput
  let p1 = partOne state
  putStrLn $ "Day 04 [Part 1] = " ++ (show p1) 

  let p2 = partTwo state
  putStrLn $ "Day 04 [Part 2] = " ++ (show p2) 
