module Solution.Day03 (main) where

import Advent.Input
import qualified Data.Text as T
import Data.Char (digitToInt)
import Data.List (tails)
import Data.Foldable (for_)

joltages :: Int -> [Int] -> [Int]
joltages n xs
  | null prefix = []
  | n == 1      = [maximum xs]
  | otherwise   = [ x*10^(n-1) + j
                  | x:ys <- take (len - n + 1) $ tails xs
                  , x == lead
                  , j <- joltages (n-1) ys]
  where
    len    = length xs
    prefix = take (len - n + 1) xs
    lead   = maximum prefix

main :: IO ()
main = do
  inp <- getInput 3
  let banks = map (map digitToInt . T.unpack) $ T.lines inp
  for_ [2,12] $ \n -> print (sum $ map (maximum . joltages n) banks)