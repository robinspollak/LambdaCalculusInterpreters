module Eval where

import Control.Applicative
import Control.Monad()
import Control.Monad.IO.Class()

import Data.Array.IO()

import Data

interp :: LC -> Either Error LC
interp = evalCBV

evalCBV :: LC -> Either Error LC
evalCBV (Var x)       = Left $ UnboundVariables [x]
evalCBV (Num n)       = Right $ Num n
evalCBV (Lambda x e)  = Right $ Lambda x e
evalCBV (Let v e1 e2) = case evalCBV e1 of
                          Right (Var v) -> evalCBV $ subst e2 v (Var v)
                          Right e1'     -> evalCBV $ subst e2 v e1'
                          Left e        -> Left e
evalCBV Succ               = Right Succ
evalCBV (App Succ (Num n)) = Right $ Num (n+1)
evalCBV (App Succ x)       = evalCBV =<< (App Succ) <$> (evalCBV x)
evalCBV (App e1 e2)   = case evalCBV e1 of
                         (Right (Lambda x e')) -> case evalCBV e2 of
                                                  Right (Var v) -> evalCBV $ subst e' x (Var v)
                                                  Right e2'     -> evalCBV $ subst e' x e2'
                                                  Left e        -> Left e
                         (Right Succ)          -> evalCBV =<< (App Succ) <$> (evalCBV e2)
                         (Right x)             -> Left $ AppliedNonFunction x
                         (Left e)              -> Left e

subst :: LC -> VarName -> LC -> LC
subst (Var e) v sub          | e == v     = sub
                             | otherwise  = Var e
subst (App l1 l2) v sub                   = App (subst l1 v sub) (subst l2 v sub)
subst Succ _ _                            = Succ
subst (Num x) _ _                         = Num x
subst e@(Lambda v1 l) v2 sub | v1 == v2   = e
                             | otherwise  = Lambda v1 $ subst l v2 sub
subst (Let v1 l1 l2) v2 sub  | v1 == v2   = Let v1 l1 (subst l2 v2 sub)
                             | otherwise  = Let v1 (subst l1 v2 sub) (subst l2 v2 sub)
