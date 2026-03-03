module ZipTree(Tree(..)) where

data Tree a =
      Leaf
    | Node(Tree a,a,Tree a)
   deriving (Eq,Show)

instance Functor Tree where
   fmap f Leaf          = Leaf
   fmap f (Node(l,v,r)) = Node(fmap f l,f v,fmap f r)

instance Applicative Tree where
    pure x = Node(pure x,x,pure x)
    (<*>) Leaf _ = Leaf
    (<*>) _ Leaf = Leaf
    (<*>) (Node (fl,fv,fr)) (Node (l,v,r)) =  Node(fl <*> l, fv v, fr <*> r)