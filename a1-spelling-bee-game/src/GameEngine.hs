{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module GameEngine
    (   Score (..),
        toCandidateBasis,
        extractBases,
        basisToPuzzle,
        isWordCorrect,
        allAnswers,
        finalScore,
        cheat
    ) where

import Helpers
import Prelude hiding (foldl, foldr, init, map, length, filter, bind, (>>=), (>>), return, fail, null, concat, concatMap, (!!))

data Score = Zero | Bad | OK | Good | Great | Perfect
    deriving (Eq, Show)

type Dictionary = [String]
type Basis = [Char]
type Puzzle = (Char,[Char])

toCandidateBasis :: String -> Maybe Basis
toCandidateBasis s = 
    let 
        basis = dedupAndSort s
    in 
        if length basis /= 7 
        then Nothing
        else Just basis

extractBases :: [String] -> [String]
extractBases dict = 
    uniqueBases
    where
        bases = filterMap toCandidateBasis dict
        groupedBases = group bases
        uniqueGroups = filter (\g -> length g == 1) groupedBases
        uniqueBases = map head uniqueGroups

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (h:t) 0 = t
removeAt (h:t) i = h:removeAt t (i-1)

getCharAt :: [a] -> Int -> a
getCharAt [] _ = error "index out of bounds"  
getCharAt (h:_) 0 = h                        
getCharAt (_:t) i = getCharAt t (i-1)       

basisToPuzzle :: Basis -> Int -> Puzzle
basisToPuzzle word index = (charAt,remainingChars)
    where 
        charAt = getCharAt word index
        remainingChars = removeAt word index

isWordCorrect :: Dictionary -> Puzzle -> String -> Bool
isWordCorrect dict (special,remaining) str =
    exists (== str) dict 
    && 
    forAll (\c -> exists (== c) (special:remaining)) str
    &&
    exists (== special) str 

allAnswers :: Dictionary -> Puzzle -> [String]
allAnswers d p = filter (\w -> isWordCorrect d p w) d

finalScore :: Dictionary -> Puzzle -> [String] -> Score
finalScore dict puzzle guessedList 
    | p == 0 = Zero
    | p < 0.25 = Bad
    | p < 0.5 = OK
    | p < 0.75 = Good
    | otherwise = Perfect
    where 
        maxScore = length (allAnswers dict puzzle)
        userScore = length guessedList
        p = divide userScore maxScore

guessesOfSize :: Int -> Puzzle -> [String]
guessesOfSize i (special,remaining) =
    filter (exists (== special)) guesses
  where
    guesses = foldr f [""] [1..i]
    f _ acc = concatMap (\c -> map (c:) acc) (special:remaining)

cheat :: (Puzzle -> String -> Bool) -> Int -> Puzzle -> [String]
cheat pred i puzzle =
    concatMap allGuesses [1..i]
    where 
        allGuesses n = filter (pred puzzle) (guessesOfSize n puzzle) 