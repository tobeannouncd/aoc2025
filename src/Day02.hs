{-# LANGUAGE OverloadedStrings #-}
module Day02 (main) where

import Lib

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Foldable (for_)
import Data.List (isPrefixOf)

parseRange :: T.Text -> (Int, Int)
parseRange txt
  | Right (a,txt1) <- TR.decimal txt
  , Just ('-',txt2) <- T.uncons txt1
  , Right (b,_) <- TR.decimal txt2 = (a, b)
  | otherwise = error ("Cannot parse: " ++ show txt)

isSilly :: Int -> Bool
isSilly n =
  let s = show n
      k = length s `div` 2
  in take k s == drop k s

isSilly' :: Int -> Bool
isSilly' n = any check [1 .. k]
  where
    s = show n
    k = length s `div` 2
    check m = go (take m s) (drop m s)
      where
        go _ [] = True
        go x y = x `isPrefixOf` y && go x (drop m y)

main :: IO ()
main = do
  inp <- getInput 2
  let ranges = map parseRange (T.splitOn "," inp)
  for_ [isSilly, isSilly'] $ \f -> do
    print $ sum
      [ x
      | (a,b) <- ranges
      , x <- [a .. b]
      , f x
      ]