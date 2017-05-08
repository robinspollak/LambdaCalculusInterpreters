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
evalCBV (Lambda x lc)  = Right $ Lambda x lc
evalCBV (Let v lc1 lc2) = case evalCBV lc1 of
                            Right (Var v)  -> evalCBV $ subst lc2 v (Var v)
                            Right lc1'     -> evalCBV $ subst lc2 v lc1'
                            Left e         -> Left e
evalCBV Succ               = Right Succ
evalCBV (App Succ (Num n)) = Right $ Num (n+1)
evalCBV (App Succ x)       = evalCBV =<< (App Succ) <$> (evalCBV x)
evalCBV (App lc1 lc2)   = case evalCBV lc1 of
                           (Right (Lambda x lc')) -> case evalCBV lc2 of
                                                       Right (Var v)  -> evalCBV $ subst lc' x (Var v)
                                                       Right lc2'     -> evalCBV $ subst lc' x lc2'
                                                       Left e         -> Left e
                           (Right Succ)           -> evalCBV =<< (App Succ) <$> (evalCBV lc2)
                           (Right x)              -> Left $ AppliedNonFunction x
                           (Left e)               -> Left e

subst :: LC -> VarName -> LC -> LC
subst (Var e) v sub             | e == v     = sub
                                | otherwise   = Var e
subst (App lc1 lc2) v sub                    = App (subst lc1 v sub) (subst lc2 v sub)
subst Succ _ _                               = Succ
subst (Num x) _ _                            = Num x
subst lc@(Lambda v1 lc1) v2 sub | v1 == v2   = lc
                                | otherwise  = Lambda v1 $ subst lc1 v2 sub
subst (Let v1 lc1 lc2) v2 sub   | v1 == v2   = Let v1 lc1 (subst lc2 v2 sub)
                                | otherwise  = Let v1 (subst lc1 v2 sub) (subst lc2 v2 sub)
