{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use concat" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use sum" #-}

module ListHOFs
    ( zip,
      alternatingMap,
      sumEvens,
      flatten
    ) where

import Prelude(Int(..),Maybe(..),filter,mod,(+),(.),(==),(++))
import GHC.Base

zip :: [a] -> [b] -> Maybe [(a,b)]
zip [] [] = Just []
zip (h1:t1) (h2:t2) =
  case zip t1 t2 of
    Just zs -> Just ((h1, h2) : zs)
    Nothing -> Nothing
zip _ _ = Nothing

alternatingMap :: (a -> b) -> (a -> b) -> [a] -> [b]
alternatingMap _ _ [] = []
alternatingMap f g (h:t) = f h : alternatingMap g f t

sumEvens :: [Int] -> Int
sumEvens l = foldr (+) 0 (filter (\x -> x `mod` 2 == 0) l)

flatten :: [[a]] -> [a]
flatten l = foldr (++) [] l
