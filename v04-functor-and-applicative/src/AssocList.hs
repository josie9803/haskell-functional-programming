module AssocList(AssocList(..),doubleMap) where

data AssocList k a = 
     Nil
   | Cons(k,a,AssocList k a)
   deriving (Eq,Show)

-- This functor should transform the data of the AssocList, not the keys. 
-- For example, fmap (+1) Cons(1,1,Nil) = Cons(1,2,Nil).
instance Functor (AssocList k) where
   fmap f Nil = Nil 
   fmap f (Cons(k,a,rest)) = Cons(k, f a, fmap f rest)

-- we would like to transform the data and the keys
doubleMap :: (k -> a -> (k',a')) -> AssocList k a -> AssocList k' a'
doubleMap f Nil = Nil 
doubleMap f (Cons(k,a,rest)) = Cons(fst newtuple, snd newtuple, doubleMap f rest) 
   where newtuple = f k a 