{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module ErrJst(ErrJst(..)) where

data ErrJst e a = 
     Err e
   | Jst a
   deriving (Eq,Show)

instance Functor (ErrJst e) where
   fmap f (Err e) = Err e
   fmap f (Jst a) = Jst (f a)

instance Applicative (ErrJst e) where
   pure f = Jst f

   (<*>) (Jst f) (Jst a) = Jst (f a)
   (<*>) (Jst f) (Err a) = Err a
   (<*>) (Err f) (Jst a) = Err f
   (<*>) (Err f) (Err a) = Err f 
   