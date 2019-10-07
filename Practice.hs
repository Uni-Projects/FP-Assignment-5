module Ex_5_1
where

import Prelude

isTrue :: Bool -> Bool
isTrue x = if x == True then True else False

isFalse :: Bool -> Bool
isFalse x = if x == False then True else False

allTrue :: [Bool] -> Bool
allTrue xs = foldr (\x acc -> (&&) acc (isTrue x)) True xs

allFalse :: [Bool] -> Bool
allFalse xs = foldr (\x acc -> (&&) acc (isFalse x)) True xs

member:: (Eq a) => a -> [a] -> Bool
member el xs = foldr (\x acc -> (||) (el == x) acc) False xs

smallest :: [Int] -> Int
smallest = foldl1 (\acc el -> if (el < acc) then el else acc)

largest :: [Int] -> Int
largest = foldl1 (\acc el -> if (el > acc) then el else acc)
