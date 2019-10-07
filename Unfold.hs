module Unfold
where

import Prelude hiding (take)
import qualified Data.List as L

unfoldr :: (t -> Maybe (a, t)) -> t -> [a]
unfoldr rep seed = produce seed
  where
    produce seed = case rep seed of
       Just (a, new_seed) -> a : produce new_seed
       Nothing            -> []

apo :: (t -> Either [a] (a, t)) -> t -> [a]
apo rep seed = produce seed
  where
    produce seed = case rep seed of
       Left l     -> l
       Right(a,ns) -> a : produce ns

take :: Int -> [a] -> [a]
take n l = unfoldr (\y -> case y of
  [] -> Nothing
  x:xs -> if (length xs < (length l - n)) then Nothing else Just (x,xs)) l

filter :: (a -> Bool) -> [a] -> [a]
filter f l = unfoldr (recursive f) l
   where
    recursive f l = case l of [] -> Nothing
                              (x:xs) -> if f x then Just (x, xs) else recursive f xs

--if condition only for avoiding crashing
fibs :: [Integer]
fibs = unfoldr (\x -> if (x == 10) then Nothing else Just (fib x, x+1)) 0
     where
      fib 0 = 1
      fib 1 = 1
      fib n = fib (n-2) + fib (n-1)

--primes :: [Integer]
