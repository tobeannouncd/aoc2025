module Solution.Day04 (main) where

import Advent.Input
import qualified Data.Text.IO.Utf8 as TIO
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (tails)

type Pt = (Int, Int)

adjacent :: Pt -> [Pt]
adjacent (i,j) = [(i+x,j+y) | x <- [-1,0,1], y <- [-1,0,1], (x,y) /= (0,0)]

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f x = head [y | y:z:_ <- tails (iterate f x), y == z]

main :: IO ()
main = do
  inp <- getInput 4
  let rollLocs = Set.fromAscList
        [ (i,j)
        | (i,row) <- zip [0 :: Int ..] (T.lines inp)
        , (j,'@') <- zip [0 :: Int ..] (T.unpack row)
        ]
      neighbors locs pt = Set.size $
        Set.fromAscList (adjacent pt)
          `Set.intersection` locs
      accessible locs = Set.filter (\x -> neighbors locs x < 4) locs
      step locs = locs `Set.difference` accessible locs
      locs' = untilStable step rollLocs
  print (Set.size $ accessible rollLocs)
  print (Set.size rollLocs - Set.size locs')