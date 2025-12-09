module Main (main) where

import Options.Applicative

import qualified Solution.Day01 as Day01
import qualified Solution.Day02 as Day02
import qualified Solution.Day03 as Day03
import qualified Solution.Day04 as Day04

solutions :: [(Int, IO ())]
solutions =
  [ (1, Day01.main)
  , (2, Day02.main)
  , (3, Day03.main)
  , (4, Day04.main)
  ]

main :: IO ()
main = do
  day <- execParser getDay
  case lookup day solutions of
    Nothing -> fail ("Day " ++ show day ++ " not in solutions")
    Just s  -> s

getDay :: ParserInfo Int
getDay =
  info (dayP <**> helper)
    ( fullDesc
   <> progDesc "Run a solution for AoC 2025" )

dayP :: Parser Int
dayP = argument auto
    ( metavar "DAY" )