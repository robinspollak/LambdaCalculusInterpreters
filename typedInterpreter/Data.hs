module Data where

import Control.Monad()
import Control.Monad.IO.Class()

import Data.Array.IO()
import Data.List
import Data.Map (Map)

type VarName = String

data Type = TBool | TInt | TPair Type Type | TFunction Type Type deriving (Eq)

data Error = ParseError String | AppliedNonFunction LC | PoorlyTypedApplication LC
             | UnboundVariables [VarName] | MismatchedBranches LC | NonBooleanCond LC
             | ThisShouldNotHappen LC | BadUnaryOperator LC | BadBinaryOperator LC
             | BadTypeAscription LC | IllegalRecursion LC

data LC = Var VarName | App LC LC | Lambda VarName Type LC | Let VarName LC LC
          | LetRec VarName Type LC LC | Ascr LC Type
          | LCBool Bool | If LC LC LC | Num Int | Pair LC LC 
          | UnOpLC UnOp LC | BinOpLC BinOp LC LC deriving (Eq)

data UnOp = Not | Neg | Fst | Snd deriving (Show, Eq)

data BinOp = And | Or | Plus | Minus | Times | Div | Equ | Lt deriving (Show, Eq)

type Context = Map VarName Type

instance Show Type where
  show TBool                              = "bool"
  show TInt                               = "int"
  show (TPair t1 t2)                      = "(" ++ show t1 ++ "," ++ show t2 ++ ")"
  show (TFunction t@(TFunction _ _) t3) = parens' (show t) ++ " -> " ++ show t3
  show (TFunction t1 t2)                  = show t1 ++ " -> " ++ show t2

instance Show LC where
  show = showTerm

parens' :: String -> String
parens' x = "(" ++ x ++ ")"

showTerm :: LC -> [Char]
showTerm (App x y) = (showTerm x) ++ " " ++ (showFactor y)
showTerm (Let v x y) = "let " ++ v ++ " = " ++ showTerm x ++ " in " ++ showTerm y
showTerm (LetRec v t x y) = "let rec " ++ v ++ ":" ++ show t ++ " = " ++ show x ++ " in " ++ show y
showTerm (If lc1 lc2 lc3) = "if " ++ show lc1 ++ " then " ++ show lc2 ++ " else " ++ show lc3
showTerm x = showFactor x

showLambda :: Bool -> LC -> [Char]
showLambda False (Lambda v t expr) = case expr of
                                       e@(Lambda _ _ _) -> v ++ ":" ++ show t ++ " " ++ showLambda True e
                                       x                -> v ++ ":" ++ show t ++ ". " ++ showTerm x
showLambda True (Lambda v t expr) = case expr of
                                      e@(Lambda _ _ _) -> parens' (v ++ ":" ++ show t) ++ " " ++ showLambda True e
                                      x                -> parens' (v ++ ":" ++ show t) ++ ". " ++ showTerm x
showLambda _ _                    = error "how'd u end up here mayne?"

showFactor :: LC -> [Char]
showFactor e@(Lambda _ _ (Lambda _ _ _)) = "lambda " ++ showLambda True e
showFactor e@(Lambda _ _ _) = "lambda " ++ showLambda False e
showFactor x = showAtom x

showAtom :: LC -> [Char]
showAtom (Var x)              = x
showAtom (Ascr lc t)          = "(" ++ show lc ++ ":" ++ show t ++ ")"
showAtom (Num n)              = show n
showAtom (Pair lc1 lc2)       = "(" ++ show lc1 ++ "," ++ show lc2 ++ ")"
showAtom (LCBool b)           = if b then "true" else "false"
showAtom (UnOpLC uo lc)       = showUnOp uo lc
showAtom (BinOpLC bo lc1 lc2) = showBinOp' bo lc1 lc2
showAtom x                    = parens' $ showTerm x

showUnOp :: UnOp -> LC -> [Char]
showUnOp Not lc = "not " ++ show lc
showUnOp Neg lc = "-" ++ show lc
showUnOp Fst lc = "fst " ++ show lc
showUnOp Snd lc = "snd " ++ show lc

showBinOp' bo lc1 lc2 = parens' $ showBinOp bo lc1 lc2

showBinOp :: BinOp -> LC -> LC -> [Char]
showBinOp And lc1 lc2 = show lc1 ++ " and " ++ show lc2
showBinOp Or lc1 lc2  = show lc1 ++ " or " ++ show lc2
showBinOp Plus lc1 lc2 = show lc1 ++ " + " ++ show lc2
showBinOp Minus lc1 lc2 = show lc1 ++ " - " ++ show lc2
showBinOp Times lc1 lc2 = show lc1 ++ " * " ++ show lc2
showBinOp Div lc1 lc2 = show lc1 ++ " / " ++ show lc2
showBinOp Equ lc1 lc2 = show lc1 ++ " == " ++ show lc2
showBinOp Lt lc1 lc2 = show lc1 ++ " < " ++ show lc2

instance Show Error where
  show (ParseError s)              = "Parse error: failed to parse: " ++ s ++ "."
  show (AppliedNonFunction lc)     = "Function application error: Attempted application of a non-function, tried to apply " ++ show lc ++ "."
  show (PoorlyTypedApplication lc) = "Type error: argument of incorrect type in " ++ show lc
  show (UnboundVariables vars)     = "Unbound variables error: " ++ (intercalate ", " vars) ++ " are unbound."
  show (MismatchedBranches lc)     = "Type error: mismatched branches in " ++ show lc
  show (NonBooleanCond lc)         = "Type error: non-boolean used as condition in " ++ show lc
  show (ThisShouldNotHappen lc)    = "WTF MAYNE? XD " ++ show lc
  show (BadUnaryOperator lc)       = "Type error: poorly typed use of unary operator: " ++ show lc
  show (BadBinaryOperator lc)      = "Type error: poorly typed use of binary operator: " ++ show lc
  show (BadTypeAscription lc)      = "Bad type ascription: the ascription doesn't match the type: " ++ show lc
  show (IllegalRecursion lc)       = "Illegal recursion: " ++ show lc
