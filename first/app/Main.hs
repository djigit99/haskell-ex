module Main where

import Lib

-- 3.	Реалізувати алгоритм сортування insert sort

main :: IO ()

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x < y = x : y : ys
  | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

main = do
    print $ isort [5,4,3,2,1]