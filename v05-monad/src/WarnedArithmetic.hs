module WarnedArithmetic(Warning(..),Expr(..),evaluate) where

import WarningAccumulatorMonad

data Warning = DivByZero | AddByNaN
   deriving (Show,Eq)

{- Hint: If the second input is x2, check if x2 == 0.0 -}
warningDivide :: Float -> Float -> WarningAccumulator Warning Float
warningDivide x1 x2 =
   let result = x1 / x2
   in if x2 == 0.0
      then WarningAccumulator (result, [DivByZero])
      else WarningAccumulator (result, [])

{- Hint: use the isNaN function -}
warningPlus :: Float -> Float -> WarningAccumulator Warning Float
warningPlus x1 x2 =
  let result = x1 + x2
  in if isNaN x1 || isNaN x2 || isNaN result
     then WarningAccumulator (result, [AddByNaN])
     else WarningAccumulator (result, [])

data Expr =
     Base Float
   | Divide (Expr,Expr)
   | Plus (Expr,Expr)
   deriving (Show,Eq)

evaluateHelper :: Expr -> WarningAccumulator Warning Float
evaluateHelper (Base x) = return x 
evaluateHelper (Divide (exp1,exp2)) = do 
   m <- evaluateHelper exp1
   n <- evaluateHelper exp2
   warningDivide m n
evaluateHelper (Plus (exp1,exp2)) = do 
   m <- evaluateHelper exp1
   n <- evaluateHelper exp2
   warningPlus m n

evaluate :: Expr -> (Float,[Warning])
evaluate e =
   let res = evaluateHelper e in
   (getResult res, getWarnings res)