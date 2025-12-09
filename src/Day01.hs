module Day01 (main) where

import Lib
import Data.List (mapAccumL)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

parseStep :: T.Text -> Int
parseStep step =
  case T.uncons step of
    Just ('L', n)
      | Right (x, _) <- TR.decimal n -> negate x
    Just ('R', n)
      | Right (x, _) <- TR.decimal n -> x
    _ -> 0

main :: IO ()
main = do
  inp <- getInput 1
  -- let inp = T.pack "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82" -- example
  let steps = map parseStep (T.lines inp)
      part1 = scanl (\x s -> (x + s) `mod` 100) 50 steps
      go pos 0    = (pos, [])
      go pos step =
        let clicks = take (abs step) . drop 1 $ iterate (\x -> (x + signum step) `mod` 100) pos
        in (last clicks, clicks)
      (_, part2) = mapAccumL go 50 steps
  print $ length $ filter (== 0) part1
  print $ length $ filter (== 0) (concat part2)