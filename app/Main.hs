module Main (main) where

import Options.Applicative

import qualified Day01
import qualified Day02

solutions :: [(Int, IO ())]
solutions =
  [ (1, Day01.main)
  , (2, Day02.main)
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