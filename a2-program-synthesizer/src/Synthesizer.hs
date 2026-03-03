{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Synthesizer 
    (numberSplit
    ,baseExpressionsAtSize
    ,varExpressionsAtSize
    ,notExpressionsAtSize
    ,andExpressionsAtSize
    ,orExpressionsAtSize
    ,expressionsAtSize
    ,expressionSatisfiesExamples
    ,generator
    )
     where

import Language
import Data.Maybe
import Data.List 

numberSplit :: Int -> [(Int,Int)]
numberSplit n 
    | n <= 1 = []
    | otherwise = map (\x -> (x, n - x)) [1 .. n-1]

baseExpressionsAtSize :: Int -> [Expression]
baseExpressionsAtSize size 
    | size /= 1 = []    
    | size == 1 = fmap EBase list 
    where list = [True,False]

varExpressionsAtSize :: Context -> Int -> [Expression]
varExpressionsAtSize (Context context) size 
    | size /= 1 = []
    | size == 1 = fmap EVariable context

notExpressionsAtSize :: (Int -> [Expression]) -> Int -> [Expression]
notExpressionsAtSize f k
  | k <= 0 = []
  | otherwise = fmap ENot (f (k-1))

andExpressionsAtSize :: (Int -> [Expression]) -> Int -> [Expression]
andExpressionsAtSize f 0 = []
andExpressionsAtSize f n = do
    (leftSize, rightSize) <- numberSplit (n-1)
    leftExp <- f leftSize
    rightExp <- f rightSize
    return (EAnd (leftExp,rightExp))

orExpressionsAtSize :: (Int -> [Expression]) -> Int -> [Expression]
orExpressionsAtSize f 0 = []
orExpressionsAtSize f n = do
    (leftSize, rightSize) <- numberSplit (n-1)
    leftExp <- f leftSize
    rightExp <- f rightSize
    return (EOr (leftExp,rightExp))

expressionsAtSize :: Context -> Int -> [Expression]
expressionsAtSize context i 
    | i <= 0 = []
    | otherwise = 
        baseExpressionsAtSize i
        ++ varExpressionsAtSize context i
        ++ notExpressionsAtSize (expressionsAtSize context) i
        ++ andExpressionsAtSize (expressionsAtSize context) i
        ++ orExpressionsAtSize (expressionsAtSize context) i

expressionSatisfiesExamples :: Examples -> Expression -> Bool
expressionSatisfiesExamples (Examples example) expression =
    all (\(a,bool) -> evaluate a expression == bool) example

{-  Generate an expression that satisfies the examples. Check if there are 
    examples at size 1, then at size 2, ... until either there are no 
    expressions at size max or until an expression is found that satisfies the
    examples.

    HINT: Use a helper function
    HINT: The "find" function will be useful here
    HINT: The "evaluate" function will be useful here
-}

generator :: Context -> Examples -> Int -> Maybe Expression
generator context example n = helper context example 1 n

helper :: Context -> Examples -> Int -> Int -> Maybe Expression
helper context example current maxSize
  | current > maxSize = Nothing
  | otherwise =
    let result = find (expressionSatisfiesExamples example) (expressionsAtSize context current)
    in case result of
        Just expression -> Just expression
        Nothing -> helper context example (current + 1) maxSize

