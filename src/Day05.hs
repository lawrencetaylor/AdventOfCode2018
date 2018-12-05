module Day05 where

import Common
import Data.Char

reacts :: Char -> Char -> Bool
reacts c1 c2 = (==)32 $ abs $ ord c1 - ord c2

innerReact :: Char -> String -> String
innerReact c (x:xs) | reacts c x = xs
innerReact c x =  c : x

react :: String -> String
react = foldr innerReact []

rawData :: IO String
rawData = readData "data\\Day05"

partOne :: String -> Int
partOne =  length .  react

removeUnits :: String -> (Char, Char) -> String
removeUnits s (c1, c2) = filter(\x -> x /= c1 && x /= c2) s

testStrings :: String -> [String]
testStrings input = 
  removeUnits input <$> zip ['a'..'z'] ['A'..'Z']

partTwo :: String -> Int
partTwo input = minimum $ partOne <$> testStrings input

main :: IO ()
main = do
  input <- rawData
  let p1 = partOne input
  putStrLn $ "Day 05 [Part 1] = " ++ (show p1) 

  let p2 = partTwo input
  putStrLn $ "Day 05 [Part 2] = " ++ (show p2) 
