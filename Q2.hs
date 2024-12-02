module Q2 where

import Control.Monad (replicateM)
import Debug.Trace (trace)

numLines :: Int
numLines = 1000

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

decreasing :: [Int] -> Bool
decreasing [l] = True
decreasing (l1:l2:ls)
  | diff > 3 || diff < 1 = False
  | otherwise            = decreasing (l2:ls)
  where diff = l1 - l2

increasing :: [Int] -> Bool
increasing [l] = True
increasing (l1:l2:ls)
  | diff > 3 || diff < 1 = False
  | otherwise            = increasing (l2:ls)
  where diff = l2 - l1

main :: IO ()
main = do
  lines <- replicateM numLines getLine
  let processedLines = [map (read :: String -> Int) (words line) | line <- lines]
  print (length (filter (\levels -> decreasing levels || increasing levels) processedLines))

  let candidates = [[removeAt i line | i <- [0..length line]] | line <- processedLines]
  print (length (filter (any (\report -> decreasing report || increasing report)) candidates))

  pure ()