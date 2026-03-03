module Gcds
    ( isDivisor,
      allDivisors,
      listIntersection,
      listGcd
    ) where

import Prelude(error,Int,Bool,(<),(==),(+),(-),(*),(/),mod)
import GHC.Read (list)

isDivisor :: Int -> Int -> Bool
isDivisor a b = mod b a == 0

allDivisors :: Int -> [Int]
allDivisors n = go n
  where 
    go 0 = []
    go k = 
      if isDivisor k n
      then k : go (k-1)
      else go (k-1)

listIntersection :: [Int] -> [Int] -> [Int]
listIntersection _ [] = []
listIntersection [] _ = []
listIntersection (h1:t1) (h2:t2)  
  | h1 == h2 = h1 : listIntersection t1 t2 
  | h2 < h1 = listIntersection t1 (h2:t2) --drop the bigger #
  | h1 < h2 = listIntersection (h1:t1) t2

listGcd :: Int -> Int -> Int
listGcd n1 n2 = x
  where x = myHead (listIntersection (allDivisors n1) (allDivisors n2))
myHead :: [a] -> a
myHead (x:_) = x
