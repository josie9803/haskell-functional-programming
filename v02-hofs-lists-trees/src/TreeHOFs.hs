{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module TreeHOFs
    ( Tree(..),
      treeMap,
      treeFold,
      treeHeight,
      treeSum,
      treeSizer
    ) where

data Tree a =
    Leaf
  | Node (Tree a,a,Tree a)
  deriving (Eq,Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f Leaf = Leaf
treeMap f (Node (lt, value, rt)) = 
  Node (treeMap f lt, f value, treeMap f rt)

treeFold :: (b -> a -> b -> b) -> b -> Tree a -> b
treeFold f init Leaf = init
treeFold f init (Node (lt, value, rt)) = 
  f (treeFold f init lt) value (treeFold f init rt)

treeHeight :: Tree a -> Int
treeHeight t = treeFold (\lt v rt -> 1 + max lt rt) 0 t 

treeSum :: Tree Int -> Int
treeSum t = treeFold (\lt v rt -> lt + v + rt) 0 t 

-- f :: b -> a -> b -> b
-- b = (Tree (a,Int), Int)
-- f :: (Tree (a,Int), Int) -> a -> (Tree (a,Int), Int) -> (Tree (a,Int), Int)
f ::  (Tree (a,Int), Int) -> a -> (Tree (a,Int), Int) -> (Tree (a,Int), Int)
f (lt, ltSize) value (rt, rtSize) = 
  (Node (lt, (value,sizeV), rt), sizeV)
  where 
    sizeV = 1 + ltSize + rtSize

treeSizer :: Tree a -> Tree (a,Int)
treeSizer t = fst (treeFold f (Leaf,0) t)