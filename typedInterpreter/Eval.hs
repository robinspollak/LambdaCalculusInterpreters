module Eval where

import Control.Applicative
import Control.Monad()
import Control.Monad.IO.Class()

import qualified Data.Map
import Data.Map()
import Data.Array.IO()

import Data

type EvalContext = Data.Map.Map VarName LC

removeAscr :: LC -> LC
removeAscr (Ascr lc _) = lc
removeAscr (Var x)     = Var x
removeAscr (Num n)     = Num n
removeAscr (Pair lc1 lc2) = Pair (removeAscr lc1) (removeAscr lc2)
removeAscr (UnOpLC uo lc) = UnOpLC uo (removeAscr lc)
removeAscr (BinOpLC bo lc1 lc2) = BinOpLC bo (removeAscr lc1) (removeAscr lc2)
removeAscr (LCBool x)  = LCBool x
removeAscr (Lambda x t lc) = Lambda x t (removeAscr lc)
removeAscr (Let v lc1 lc2) = Let v (removeAscr lc1) (removeAscr lc2)
removeAscr (LetRec v t lc1 lc2) = LetRec v t (removeAscr lc1) (removeAscr lc2)
removeAscr (If lc1 lc2 lc3)      = If (removeAscr lc1) (removeAscr lc2) (removeAscr lc3)
removeAscr (App lc1 lc2) = App (removeAscr lc1) (removeAscr lc2)

evalCBV ::  EvalContext -> LC -> Either Error LC
evalCBV g (Var x)         = case Data.Map.lookup x g of
                            Just x' -> Right x'  
                            Nothing -> Left $ UnboundVariables [x]
evalCBV _ (Num n)         = Right $ Num n
evalCBV g (Pair lc1 lc2)  = Pair <$> evalCBV g lc1 <*> evalCBV g lc2
evalCBV g (Ascr lc t)     = Ascr <$> evalCBV g lc <*> pure t
evalCBV g (UnOpLC uo lc)  = evalUnOp g uo lc
evalCBV g (BinOpLC bo lc1 lc2) = evalBinOp g bo lc1 lc2
evalCBV _ (LCBool x)      = Right $ LCBool x
evalCBV _ (Lambda x t lc)  = Right $ Lambda x t lc
evalCBV g (Let v lc1 lc2)   = case evalCBV g lc1 of 
                              Right (Var v) -> evalCBV g $ subst lc2 v (Var v)
                              Right lc'     -> evalCBV g $ subst lc2 v lc'
                              Left e        -> Left e
evalCBV g e@(LetRec v _ lc1 lc2) = case evalCBV (Data.Map.insert v lc1 g) lc1 of
                                   Right lam@(Lambda _ _ _) -> evalCBV (Data.Map.insert v lc1 g) $ subst lc2 v lam
                                   Right _                  -> Left $ IllegalRecursion e
                                   Left err                 -> Left err
evalCBV g (If lc1 lc2 lc3)   = case evalCBV g lc1 of
                              Right (LCBool b) -> if b then evalCBV g lc2 else evalCBV g lc3
                              Right lc         -> Left $ ThisShouldNotHappen lc
                              Left e           -> Left e 
evalCBV g (App lc1 lc2)   = case evalCBV g lc1 of
                            (Right (Lambda x _ lc)) -> case evalCBV g lc2 of
                                                       Right (Var v) -> evalCBV g $ subst lc x (Var v)
                                                       Right lc2'     -> evalCBV g $ subst lc x lc2'
                                                       Left e        -> Left e
                            (Right lc)              -> Left $ AppliedNonFunction lc
                            (Left e)                -> Left e

evalUnOp :: EvalContext -> UnOp -> LC -> Either Error LC
evalUnOp g Not lc = case evalCBV g lc of
                    Right (LCBool b) -> Right $ LCBool (not b)
                    Right _          -> Left $ ThisShouldNotHappen lc
                    Left e           -> Left e
evalUnOp g Neg lc = case evalCBV g lc of
                    Right (Num n)    -> Right $ Num (-1*n)
                    Right _          -> Left $ ThisShouldNotHappen lc
                    Left e           -> Left e
evalUnOp g Fst lc = case evalCBV g lc of
                    Right (Pair lc1 _)   -> evalCBV g lc1
                    Right _              -> Left $ ThisShouldNotHappen (UnOpLC Fst lc)
                    Left e               -> Left e
evalUnOp g Snd lc = case evalCBV g lc of
                    Right (Pair _ lc2)   -> evalCBV g lc2
                    Right _              -> Left $ ThisShouldNotHappen (UnOpLC Snd lc)
                    Left e               -> Left e

evalBinOp :: EvalContext -> BinOp -> LC -> LC -> Either Error LC
evalBinOp g Equ lc1 lc2                      = do
                                               lc1' <- evalCBV g lc1
                                               lc2' <- evalCBV g lc2
                                               case lc1' of
                                                 (Lambda _ _ _) -> Left $ BadBinaryOperator (BinOpLC Equ lc1 lc2)
                                                 _              -> case lc2' of
                                                                     (Lambda _ _ _) -> Left $ BadBinaryOperator (BinOpLC Equ lc1 lc2)
                                                                     _              -> Right $ LCBool (lc1' == lc2')
evalBinOp g bo lc1 lc2 | bo `elem` [And, Or] = case evalCBV g lc1 of
                           Right (LCBool b) -> case evalCBV g lc2 of
                                               Right (LCBool b') -> Right $ LCBool ((convertBoolOp bo) b b')
                                               Right badlc       -> Left $ ThisShouldNotHappen badlc
                                               Left e            -> Left e
                           Right badlc'     -> Left $ ThisShouldNotHappen badlc'
                           Left e           -> Left e
evalBinOp g bo lc1 lc2 | bo `elem` [Plus, Minus, Times, Div] = case evalCBV g lc1 of
                           Right (Num n)    -> case evalCBV g lc2 of
                                               Right (Num n') -> Right $ Num ((convertIntOp bo) n n')
                                               Right badlc       -> Left $ ThisShouldNotHappen badlc
                                               Left e            -> Left e
                           Right badlc'     -> Left $ ThisShouldNotHappen badlc'
                           Left e           -> Left e
evalBinOp g Lt lc1 lc2                       = case evalCBV g lc1 of
                           Right (Num n)    -> case evalCBV g lc2 of
                                               Right (Num n') -> Right $ LCBool (n < n')
                                               Right badlc       -> Left $ ThisShouldNotHappen badlc
                                               Left e            -> Left e
                           Right badlc'     -> Left $ ThisShouldNotHappen badlc'
                           Left e           -> Left e
evalBinOp _ bo lc1 lc2                       = Left $ ThisShouldNotHappen (BinOpLC bo lc1 lc2)

convertBoolOp :: BinOp -> Bool -> Bool -> Bool
convertBoolOp And = (&&)
convertBoolOp Or = (||)
convertBoolOp _  = undefined

convertIntOp :: BinOp -> Int -> Int -> Int
convertIntOp Plus = (+)
convertIntOp Minus = (-)
convertIntOp Times = (*)
convertIntOp Div = (div)
convertIntOp _   = undefined
                        

subst :: LC -> VarName -> LC -> LC
subst (Var e) v sub            | e == v     = sub
                               | otherwise  = Var e
subst (Num n) _ _                           = Num n
subst (Pair lc1 lc2) v sub                  = Pair (subst lc1 v sub) (subst lc2 v sub)                  
subst (Ascr lc t) v sub                     = Ascr (subst lc v sub) t
subst (LCBool b) _ _                        = LCBool b
subst (UnOpLC uo lc) v sub                  = UnOpLC uo $ subst lc v sub
subst (BinOpLC bo lc1 lc2) v sub            = BinOpLC bo (subst lc1 v sub) (subst lc2 v sub)
subst (App l1 l2) v sub                     = App (subst l1 v sub) (subst l2 v sub)
subst e@(Lambda v1 t l) v2 sub | v1 == v2   = e
                               | otherwise  = Lambda v1 t $ subst l v2 sub
subst (If lc1 lc2 lc3) v sub                = If (subst lc1 v sub) (subst lc2 v sub) (subst lc3 v sub) 
subst (Let v1 l1 l2) v2 sub    | v1 == v2   = Let v1 l1 (subst l2 v2 sub)
                               | otherwise  = Let v1 (subst l1 v2 sub) (subst l2 v2 sub)
subst (LetRec v1 t lc1 lc2) v2 sub | v1 == v2  = LetRec v1 t lc1 (subst lc2 v2 sub)
                                   | otherwise = LetRec v1 t (subst lc1 v2 sub) (subst lc2 v2 sub)
