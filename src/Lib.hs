{-# LANGUAGE OverloadedStrings #-}
module Lib (getInput) where

import Data.Text (Text, stripEnd)
import Advent
import System.Environment

getInput :: Integer -> IO Text
getInput day =
  case mkDay day of
    Nothing -> fail ("Invalid day: " ++ show day)
    Just d  -> do
      opts <- myOpts
      result <- runAoC opts $ AoCInput d
      case result of
        Left err  -> fail (show err)
        Right inp -> return (stripEnd inp)

myOpts :: IO AoCOpts
myOpts = do
  sess <- getEnv "AOC"
  return $
    defaultAoCOpts
      (AoCUserAgent "tobeannouncd/aoc2025" "tobeannouncd@gmail.com")
      2025
      sess