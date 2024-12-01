module Q1 where

import Data.List (sort)
import Control.Monad (replicateM)

-- current implementation: sort the lists, then sum zipWith mod difference

numLines :: Int
numLines = 1000

distance :: [Int] -> [Int] -> Int
distance lefts rights = sum (zipWith (\a b -> abs (a - b)) (sort lefts) (sort rights))

similarity :: [Int] -> [Int] -> Int
similarity lefts rights = sum [sum [left | right <- rights, left == right] | left <- lefts]

main :: IO ()
main = do
  lines <- replicateM numLines getLine
  let pairs = [let [l,r] = map (read :: String -> Int) (words line) in (l,r) | line <- lines]

  let (lefts, rights) = unzip pairs

  print (distance lefts rights)
  print (similarity lefts rights)

  pure ()