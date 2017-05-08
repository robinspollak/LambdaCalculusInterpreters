module Type where

import Control.Monad()
import Control.Applicative
import Control.Monad.IO.Class()

import Data.Array.IO()
import qualified Data.Map as Map

import Data


typeOf :: Context -> LC -> Either Error Type
typeOf g (Var v)           = case v `Map.lookup` g of
                             Just x  -> Right x
                             Nothing -> Left $ UnboundVariables [v]
typeOf _ (Num _)           = Right TInt
typeOf _ (LCBool _)        = Right TBool
typeOf g (Ascr lc t)       = do
                             t1 <- typeOf g lc
                             if t1 == t
                             then return t
                             else Left $ BadTypeAscription (Ascr lc t)
typeOf g (UnOpLC uo lc)    = typeOfUnOp g uo lc
typeOf g (Pair lc1 lc2)    = TPair <$> typeOf g lc1 <*> typeOf g lc2
typeOf g (BinOpLC bo lc1 lc2) = typeOfBinOp g bo lc1 lc2
typeOf g e@(App lc1 lc2)   = case typeOf g lc1 of
                             Right (TFunction t1 t2) -> case typeOf g lc2 of
                                                         Right t3 | t1 == t3  -> Right t2
                                                                  | otherwise -> Left $ PoorlyTypedApplication e
                                                         Left e               -> Left e
                             Right _                 -> Left $ AppliedNonFunction e
                             Left e                  -> Left e
typeOf g e@(If lc1 lc2 lc3) = do
                              t1 <- typeOf g lc1
                              t2 <- typeOf g lc2
                              t3 <- typeOf g lc3
                              if t1 == TBool
                              then if t2 == t3
                                   then Right t2
                                   else Left $ MismatchedBranches e
                              else Left $ NonBooleanCond e
typeOf g (Lambda v t lc) = TFunction t <$> typeOf (Map.insert v t g) lc
typeOf g (Let v lc1 lc2) = flip typeOf lc2 =<< Map.insert v <$> (typeOf g lc1) <*> pure g
typeOf g e@(LetRec v t lc1 lc2) = case typeOf (Map.insert v t g) lc1 of
                                  Right t1@(TFunction _ _) | t1 == t     -> typeOf (Map.insert v t1 g) lc2
                                  Right (TFunction _ _)    | otherwise   -> Left $ BadTypeAscription e
                                  Right _                                -> Left $ IllegalRecursion e
                                  Left e                                 -> Left e

typeOfUnOp :: Context -> UnOp -> LC -> Either Error Type
typeOfUnOp g Not lc = case typeOf g lc of
                      Right TBool -> Right TBool
                      Right _     -> Left $ BadUnaryOperator (UnOpLC Not lc)
                      Left e      -> Left e
typeOfUnOp g Neg lc = case typeOf g lc of
                      Right TInt -> Right TInt
                      Right _    -> Left $ BadUnaryOperator (UnOpLC Neg lc)
                      Left e     -> Left e
typeOfUnOp g Fst lc = case typeOf g lc of
                      Right (TPair t1 _) -> Right t1
                      Right _            -> Left $ BadUnaryOperator (UnOpLC Fst lc)
                      Left e             -> Left e
typeOfUnOp g Snd lc = case typeOf g lc of
                      Right (TPair _ t2) -> Right t2
                      Right _            -> Left $ BadUnaryOperator (UnOpLC Snd lc)
                      Left e             -> Left e

typeOfBinOp :: Context -> BinOp -> LC -> LC -> Either Error Type
typeOfBinOp g Equ lc1 lc2 = do
                            lc1' <- typeOf g lc1
                            lc2' <- typeOf g lc2 
                            if (lc1' == lc2')
                            then case lc1' of
                                 (TFunction _ _) -> Left $ BadBinaryOperator (BinOpLC Equ lc1 lc2)
                                 _               -> Right TBool
                            else Left $ BadBinaryOperator (BinOpLC Equ lc1 lc2)
typeOfBinOp g bo lc1 lc2 | bo `elem` [And, Or] = do
                            lc1' <- typeOf g lc1
                            lc2' <- typeOf g lc2
                            if (lc1' == TBool && lc2' == TBool)
                            then Right TBool
                            else Left $ BadBinaryOperator (BinOpLC bo lc1 lc2)
typeOfBinOp g bo lc1 lc2 | bo `elem` [Plus, Minus, Times, Div] = do
                            lc1' <- typeOf g lc1
                            lc2' <- typeOf g lc2
                            if (lc1' == TInt && lc2' == TInt)
                            then Right TInt
                            else Left $ BadBinaryOperator (BinOpLC bo lc1 lc2)
typeOfBinOp g Lt lc1 lc2                       = do
                            lc1' <- typeOf g lc1
                            lc2' <- typeOf g lc2 
                            if (lc1' == TInt && lc2' == TInt)
                            then Right TBool
                            else Left $ BadBinaryOperator (BinOpLC Lt lc1 lc2)
typeOfBinOp _ bo lc1 lc2                       = Left $ ThisShouldNotHappen (BinOpLC bo lc1 lc2)

typeCheck :: LC -> Either Error LC
typeCheck lc = case typeOf Map.empty lc of
                 Right _ -> Right lc
                 Left  e  -> Left e
