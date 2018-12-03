module Common(
  readData
, pInt
, Common.parse
) where

import System.FilePath
import System.Directory
import Text.ParserCombinators.Parsec(Parser, ParseError)
import Text.Parsec as P

readData :: String -> IO String
readData relativePath =
  flip combine relativePath <$> getCurrentDirectory
  >>= readFile

pInt :: Parser Int
pInt = 
  (read <$> many1 digit :: Parser Int)

parse :: Parser a -> String -> Either ParseError a
parse parser = P.parse parser []