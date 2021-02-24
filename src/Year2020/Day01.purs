module Year2020.Day01 where

import Control.MonadZero (guard)
import Data.Array (head, mapMaybe)
import Data.Function (($))
import Data.Int (toStringAs, decimal, fromString)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Data.String.Utils (lines)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)
import Prelude ((==), (+), (*), bind, discard, pure)

sample_input :: Array Int
sample_input =
  [
    1721,
    979,
    366,
    299,
    675,
    1456
  ]
--------------------------------------------------------------------------------
-- Write your solutions here

part1 :: String -> Effect Unit
part1 input = do
  let result = toStringAs decimal $ fromMaybe 0 $ head $ findPair 2020 sample_input
  log $ "Part 1 ==> " <> result

part2 :: String -> Effect Unit
part2 input = do
  content <- readTextFile UTF8 "input/year2020/day1"
  let numbers = mapMaybe fromString $ lines content
  let result = toStringAs decimal $ fromMaybe 0 $ head $ findPair 2020 numbers
  log $ "Part 2 ==> " <> result

--------------------------------------------------------------------------------

findPair :: Int -> Array Int -> Array Int
findPair target input = do
  a <- input
  b <- input
  guard (a + b == target)
  pure $ a * b

findTriple :: Int -> Array Int -> Array Int
findTriple target input = do
  a <- input
  b <- input
  c <- input
  guard (a + b + c == target)
  pure $ a * b * c
