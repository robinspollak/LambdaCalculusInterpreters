module Data where

import Control.Monad()
import Control.Monad.IO.Class()

import Data.Array.IO()
import Data.List

type VarName = String

data LC = Var VarName | App LC LC | Lambda VarName LC | Let VarName LC LC | Num Int | Succ

instance Show LC where
  show = showTerm

data Error = ParseError String | AppliedNonFunction LC | UnboundVariables [VarName] | BadChurch LC

instance Show Error where
  show (ParseError s)          = "Parse error: failed to parse: " ++ s ++ "."
  show (AppliedNonFunction lc) = "Function application error: Attempted application of a non-function, tried to apply " ++ show lc ++ "."
  show (UnboundVariables vars) = "Unbound variables error: " ++ (intercalate ", " vars) ++ " are unbound." 
  show (BadChurch lc)          = "Church conversion error: failed while trying to convert " ++ show lc ++ " to a church numeral."

parens' :: String -> String
parens' x = "(" ++ x ++ ")"

showTerm :: LC -> [Char]
showTerm (App x y) = (showTerm x) ++ " " ++ (showFactor y)
showTerm (Let v x y) = "let " ++ v ++ " = " ++ showTerm x ++ " in " ++ showTerm y
showTerm x = showFactor x

showLambda :: LC -> [Char]
showLambda (Lambda v expr) = case expr of
                             e@(Lambda _ _) -> v ++ " " ++ showLambda e
                             x              -> v ++ ". " ++ showTerm x
showLambda _               = error "how'd u end up here mayne?"

showFactor :: LC -> [Char]
showFactor e@(Lambda _ _) = "lambda " ++ showLambda e
showFactor x = showAtom x

showAtom :: LC -> [Char]
showAtom (Succ)  = "succ"
showAtom (Var x) = x
showAtom (Num n) = show n
showAtom x = parens' $ showTerm x
