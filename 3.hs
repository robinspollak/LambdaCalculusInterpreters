{-# OPTIONS_GHC -W #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Hw03 where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)

type VarName = String

type Store = Map VarName Int

data AExp =
    Var VarName
  | Num Int
  | Plus AExp AExp
  | Times AExp AExp
  | Neg AExp
  deriving (Show, Eq, Ord)

evalA :: Store -> AExp -> Int
evalA st (Var var) = evalA st (Num $ Map.findWithDefault 0 var st)
evalA _ (Num x) = x
evalA st (Plus e1 e2) = (evalA st e1) + (evalA st e2)
evalA st (Times e1 e2) = (evalA st e1) * (evalA st e2)
evalA st (Neg e1) = negate $ evalA st e1

data BExp a =
    Bool Bool
  | Equal a a
  | Lt a a
  | Not (BExp a)
  | Or (BExp a) (BExp a)
  | And (BExp a) (BExp a)
  deriving (Show, Eq, Ord)

evalB :: Store -> BExp AExp -> Bool
evalB _ (Bool b) = b
evalB st (Equal e1 e2) = evalA st e1 == evalA st e2
evalB st (Lt e1 e2) = evalA st e1 < evalA st e2
evalB st (Not e1) = not $ evalB st e1
evalB st (Or e1 e2) = evalB st e1 || evalB st e2
evalB st (And e1 e2) = evalB st e1 && evalB st e2

data Stmt a b =
    Skip
  | Assign VarName a
  | Seq (Stmt a b) (Stmt a b)
  | If (b a) (Stmt a b) (Stmt a b)
  | While (b a) (Stmt a b)
  deriving (Show, Eq, Ord)

eval :: Store -> Stmt AExp BExp -> Store
eval st Skip = st
eval st (Assign name var) = Map.insert name (evalA st var) st
eval st (Seq e1 e2) = eval (eval st e1) e2
eval st (If cond e1 e2) = if (evalB st cond) then (eval st e1) else (eval st e2)
eval st (While cond e1) = if (evalB st cond) then eval st (Seq e1 (While cond e1)) else st

data AExp' =
    Var' VarName
  | Num' Int
  | Plus' AExp' AExp'
  | Times' AExp' AExp'
  | Neg' AExp'
  | Div' AExp' AExp'
  deriving (Show, Eq)

evalFMap :: (a -> b) -> Either Error a -> Either Error b
evalFMap _ (Left err) = Left err
evalFMap f (Right a) = Right $ f a

twoEitherFMap :: (a -> b -> c) -> Either Error a -> Either Error b -> Either Error c
twoEitherFMap _ (Left err) _ = Left err
twoEitherFMap _ _ (Left err) = Left err
twoEitherFMap f (Right a) (Right b) = Right (f a b)

oneEitherFMap :: (a -> b -> c) -> a -> Either Error b -> Either Error c
oneEitherFMap _ _ (Left err) = Left err
oneEitherFMap f a (Right b) = Right (f a b)

oneEitherFMap' :: (a -> b -> Either Error c) -> Either Error a -> b -> Either Error c
oneEitherFMap' _ (Left err) _  = (Left err)
oneEitherFMap' f (Right a) b = (f a b)

evalA' :: Store -> AExp' -> Either Error Int
evalA' st (Var' var) = case Map.lookup var st of Nothing -> Left $ NoSuchVariable var
                                                 Just x  -> evalA' st (Num' x) 
evalA' _ (Num' x) = Right x
evalA' st (Plus' e1 e2) = twoEitherFMap (+) (evalA' st e1) (evalA' st e2)
evalA' st (Times' e1 e2) = twoEitherFMap (*) (evalA' st e1) (evalA' st e2)
evalA' st (Neg' e1) = evalFMap negate $ evalA' st e1
evalA' st e0@(Div' e1 e2) = case evalA' st e2 of (Left err) -> (Left err)
                                                 (Right 0)  -> (Left $ DivideByZero e0)
                                                 res        -> twoEitherFMap div (evalA' st e1) res

evalB' :: Store -> BExp AExp' -> Either Error Bool
evalB' _ (Bool b) = Right b
evalB' st (Equal e1 e2) = twoEitherFMap (==) (evalA' st e1) $ evalA' st e2
evalB' st (Lt e1 e2) = twoEitherFMap (<) (evalA' st e1) $ evalA' st e2
evalB' st (Not e1) = evalFMap not $ evalB' st e1
evalB' st (Or e1 e2) = twoEitherFMap (||) (evalB' st e1) $ evalB' st e2
evalB' st (And e1 e2) = twoEitherFMap (&&) (evalB' st e1) $ evalB' st e2

oneEitherThreeArgFMap :: (a -> b -> c -> d) -> a -> Either Error b -> c -> Either Error d
oneEitherThreeArgFMap _ _ (Left err) _ = Left err
oneEitherThreeArgFMap f a (Right b) c = Right (f a b c)

data Error = NoSuchVariable VarName | DivideByZero AExp' deriving Show

test0 = Assign "foo" (Num' 1)
test1 = Assign "foo" (Times' (Num' 2) (Var' "foo"))
test2 = Assign "foo" (Neg' (Var' "foo"))
test4 = Seq test0 (Seq (While (Lt (Var' "foo") (Num' 20)) test1) test2)



eval' :: Store -> Stmt AExp' BExp -> Either Error Store
eval' st Skip = Right st
eval' st (Assign name e1) = oneEitherThreeArgFMap Map.insert name (evalA' st e1) st
eval' st (Seq e1 e2) = case eval' st e1 of (Left err)  -> (Left err)
                                           (Right st2) -> eval' st2 e2
eval' st (If cond e1 e2) = case evalB' st cond of (Left err)    -> Left err
                                                  (Right True)  -> eval' st e1
                                                  (Right False) -> eval' st e2
eval' st (While cond e1) = case evalB' st cond of (Left err)    -> Left err
                                                  (Right True)  -> eval' st (Seq e1 (While cond e1))
                                                  (Right False) -> Right st

varsA :: AExp' -> Set VarName
varsA (Var' var) = Set.singleton var
varsA (Num' _) = Set.empty
varsA (Plus' e1 e2) = Set.union (varsA e1) $ varsA e2
varsA (Times' e1 e2) = Set.union (varsA e1) $ varsA e2
varsA (Neg' e1) = varsA e1
varsA (Div' e1 e2) = Set.union (varsA e1) $ varsA e2

varTest0 = Var' "foo"
varTest1 = Plus' varTest0 (Var' "bar")
varTest2 = Plus' (Neg' (Var' "foobar")) varTest1

varsB :: BExp AExp' -> Set VarName
varsB (Bool _) = Set.empty
varsB (Equal e1 e2) = Set.union (varsA e1) $ varsA e2
varsB (Lt e1 e2) = Set.union (varsA e1) $ varsA e2
varsB (Not e1) = varsB e1
varsB (Or e1 e2) =  Set.union (varsB e1) $ varsB e2
varsB (And e1 e2) =  Set.union (varsB e1) $ varsB e2

useBeforeDef :: Set VarName -> Stmt AExp' BExp -> (Set VarName, Set VarName)
useBeforeDef defs Skip = (defs, Set.empty)
useBeforeDef defs (Assign x a) = (Set.insert x defs, varsA a `Set.difference` defs)
useBeforeDef defs (Seq e1 e2) = let (newDefs, unbound) = useBeforeDef defs e1
                                    (newNewDefs, newUnbound) = useBeforeDef newDefs e2
                                in (newNewDefs, Set.union unbound newUnbound)
useBeforeDef defs (If b e1 e2) = let (e1Defs, e1Unbound) = useBeforeDef defs e1
                                     (e2Defs, e2Unbound) = useBeforeDef defs e2
                                 in (Set.intersection e1Defs e2Defs, (Set.union e1Unbound e2Unbound) `Set.union` (varsB b `Set.difference` defs))
useBeforeDef defs (While b e1) = let (_, e1Unbound) = useBeforeDef defs e1 in (defs, e1Unbound `Set.union` (varsB b `Set.difference` defs))


testUnbound = useBeforeDef Set.empty unboundY
testAmbiguous = useBeforeDef Set.empty (ambiguous (Bool True))

unboundY = Assign "x" (Var' "y")
ambiguous b = Seq (If b (Assign "y" (Num' 0)) Skip) unboundY
test5 = If (Lt (Num' 4) (Var' "hello")) (While (Lt (Num' 5) (Var' "x")) (Assign "y" (Num' 0))) (While (Lt (Num' 5) (Var' "y")) (Assign "x" (Num' 0)))
test6 = If (Lt (Num' 4) (Var' "hello")) (Assign "y" (Num' 0)) (Assign "y" (Num' 0))
test7 = Seq (If (Lt (Num' 4) (Var' "hello")) (Assign "y" (Num' 0)) (Assign "y" (Num' 0))) (Assign "y" (Var' "x"))
test8 = Seq (If (Lt (Num' 4) (Var' "hello")) (Assign "y" (Num' 0)) (Assign "y" (Num' 0))) (Assign "x" (Var' "y"))

type Config = (Store, Stmt AExp BExp)

step :: Config -> Maybe Config
step (_,Skip) = Nothing
step (st,Assign x a) = Just (Map.insert x (evalA st a) st,Skip)
step (st,Seq Skip s2) = Just (st,s2)
step (st,Seq s1 s2) = let evaluated = step (st, s1)
                      in case evaluated of Nothing           -> Just (st, s2)
                                           Just (nst, nstmt) -> Just (nst, Seq nstmt s2)
step (st,If b s1 s2) = if evalB st b then step (st, s1) else step (st, s2)
step (st,While b s) = if evalB st b then step (st, Seq s (While b s)) else Nothing

trace :: (a -> Maybe a) -> a -> [a]
trace f v =
  case f v of
    Nothing -> [v]
    Just v' -> v:trace f v'

data TVL = No | Maybe | Yes deriving (Show, Eq, Ord)

diverges :: Ord a => Int -> [a] -> TVL
diverges limit l = divergesHelp l Set.empty
                   where divergesHelp [] _ = No
                         divergesHelp (x:xs) set = let newSet = Set.insert x set
                                                       newSetLength = length newSet 
                                                       in if length set == newSetLength
                                                          then Yes
                                                          else if newSetLength >= limit
                                                               then Maybe
                                                               else divergesHelp xs newSet

haltsIn :: Stmt AExp BExp -> Int -> TVL
haltsIn s limit = haltsInHelp (Map.empty, s) []
                  where haltsInHelp config l = case step config of Nothing -> Yes
                                                                   Just newConfig -> let newList = (config:l)
                                                                                     in case diverges limit newList of Yes   -> No
                                                                                                                       No    -> haltsInHelp newConfig (config:l)
                                                                                                                       Maybe -> Maybe

test9 = Seq (If (Lt (Num 4) (Var "hello")) (Assign "y" (Num 0)) (Assign "y" (Num 0))) (Assign "y" (Var "x"))
test10 = Seq (If (Lt (Num 4) (Var "hello")) (Assign "y" (Num 0)) (Assign "y" (Num 0))) (Assign "x" (Var "y"))

loop :: Stmt AExp BExp
loop  = Seq (Assign "x" (Num 0)) (While (Bool True) (Assign "x" (Num 4)))

long :: Stmt AExp BExp
long = Seq (Assign "x" (Num 0)) (While (Lt (Var "x") (Num 1005)) (Assign "x" (Plus (Var "x") (Num 1))))

tricky :: Stmt AExp BExp
tricky = Seq (Assign "x" (Num 0)) (While (Bool True) (Assign "x" (Plus (Var "x") (Num 1))))

--haltsIn gives an imprecise answer because the configs being generated by step will never have 
--any repeating values. This is because tricky is self referentially incrementing and thus
--x will never have a repeated value and our store will never be the same even though our instructions
--are always the same.

--haltsIn will never give an incorrect answer because we have allowed it to return a "Maybe" option
--due to it returning a TVL as opposed to a bool. The halting problem is undecidable, so if it had to
--return a bool then we would have problems. We instead force it to return an answer that is never "wrong"
--per se in the form of Maybe, even if thats kind of cheating if we're trying to "solve the halting problem"  





