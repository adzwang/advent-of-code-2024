module Q2 where

import Control.Monad (replicateM)

numLines :: Int
numLines = 1000

decreasing :: [Int] -> Bool
decreasing [l] = True
decreasing (l1:l2:ls)
  | diff > 3  = False
  | diff < 1  = False
  | otherwise = decreasing (l2:ls)
  where diff = l1 - l2

increasing :: [Int] -> Bool
increasing [l] = True
increasing (l1:l2:ls)
  | diff > 3  = False
  | diff < 1  = False
  | otherwise = increasing (l2:ls)
  where diff = l2 - l1

main :: IO ()
main = do
  lines <- replicateM numLines getLine
  print (length (filter (\levels -> decreasing levels || increasing levels) [map (read :: String -> Int) (words line) | line <- lines]))

  pure ()