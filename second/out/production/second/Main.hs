module Main where

import Lib

func1 :: [Int]->[Int]
func1 [] = []
func1 (x:xs)
    | length xs > 0 && x == xs!!0 = x:(func1 xs)
    | otherwise = func1 xs

main :: IO ()
main = do
  print $ func1 [2, 2, 3, 4, 3]
