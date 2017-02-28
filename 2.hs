module Hw02 where
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import qualified Data.List (sort)
import Data.Maybe


data ArithExp =
    Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp

parens :: String -> String
parens s = "(" ++ s ++ ")"

instance Show ArithExp where
  show (Num n) = "Num " ++ (show n)
  show (Plus x y) = "Plus " ++ parens (show x) ++ parens (show y)  
  show (Times x y) = "Times " ++ parens (show x) ++ parens (show y) 
  show (Neg n)= "Neg " ++ parens (show n)

instance Eq ArithExp where
    Num a == Num b = a == b
    Plus  w x == Plus y z = (w == y && x == z) || (w == z && x == y) 
    Times  w x == Times y z = (w == y && x == z) || (w == z && x == y) 
    Neg a == Neg b = a == b
    _ == _  = False

eval :: ArithExp -> Int
eval (Num n) = n
eval (Plus x y) = eval x + eval y
eval (Times x y) = eval x * eval y
eval (Neg n) = -(eval n)

data ArithExp' =
    Num' Int
  | Plus' ArithExp' ArithExp'
  | Sub' ArithExp' ArithExp'
  | Times' ArithExp' ArithExp'
  | Neg' ArithExp'
  deriving Show

eval' :: ArithExp' -> Int
eval' = eval . translate

translate :: ArithExp' -> ArithExp
translate (Num' n) = Num n
translate (Plus' x y) = Plus (translate x) (translate y)
translate (Sub' x y) = Plus (translate x) (Neg (translate y))
translate (Times' x y) = Times (translate x) (translate y)
translate (Neg' x) = Neg (translate x)

instance Eq ArithExp' where
    x == y = eval' x == eval' y

instance Ord ArithExp' where
  compare e1 e2 = compare (eval' e1) (eval' e2)

class Setlike f where
  emp :: f a

  singleton :: a -> f a

  union :: Ord a => f a -> f a -> f a
  union = fold insert

  insert :: Ord a => a -> f a -> f a
  insert = union . singleton

  delete :: Ord a => a -> f a -> f a
  delete x s = fold (\y s' -> if x == y then s' else insert y s') emp s

  isEmpty :: f a -> Bool
  isEmpty = (==0) . size

  size :: f a -> Int
  size = fold (\_ count -> count + 1) 0

  isIn :: Ord a => a -> f a -> Bool
  isIn x s = maybe False (const True) $ getElem x s

  getElem :: Ord a => a -> f a -> Maybe a

  fold :: (a -> b -> b) -> b -> f a -> b

  toAscList :: f a -> [a] -- must return the list sorted ascending
  toAscList = fold (:) []

instance  Setlike [] where
  emp = []

  singleton a = [a]

  insert a [] = singleton a
  insert a (x:xs) | a > x = x : insert a xs
                  | otherwise = a:x:xs

  isIn = elem

  fold = foldr

  getElem a [] = Nothing
  getElem a (x:xs) | a == x = Just x
                   | otherwise = getElem a xs

  toAscList l = l

evensUpToTen :: [Int]
evensUpToTen = foldr insert emp [0,2,4,6,8]

data BST a = Empty | Node (BST a) a (BST a)

bstsmall = Node Empty 1 Empty
bst = Node (Node bstsmall 2 (Node Empty 3 Empty)) 4 Empty


instance Setlike BST where

  emp = Empty

  singleton a = Node (Empty) a (Empty)

  insert y Empty = singleton y
  insert y (Node l x r) | x > y = Node (insert y l) x r
                        | x < y = Node l x (insert y r)
                        |otherwise = Node l x r

  delete _ Empty = Empty
  delete x (Node Empty m Empty) | x == m = Empty
  delete x (Node l m Empty)     | x == m = l
  delete x (Node Empty m r)     | x == m = r
  delete x (Node l m r)         | x == m = let suc = successor r  in (Node l suc (delete suc r))
                                | x < m  = Node (delete x l) m r
                                | x > m  = Node l m (delete x r)
                                           where
                                            successor (Node Empty m _) = m
                                            successor (Node l _ _)     = successor l
  isIn a Empty = False
  isIn a (Node l x r) | x == a = True
                      | x > a  = isIn a l
                      | x < a  = isIn a r

  getElem a Empty = Nothing
  getElem a (Node l x r) | a == x = Just x
                         | a > x = getElem a r
                         | a < x = getElem a l

  fold _ b Empty = b
  fold f b (Node l x r) =  fold f (f x (fold f b r)) l

instance Ord a => Eq (BST a) where
  Empty == Empty = True
  Empty == _ = False
  _ == Empty = False
  a == b = toAscList a == toAscList b

instance Show a => Show (BST a) where
  show Empty = "Empty"
  show (Node l x r) = "Node " ++ parens (show l) ++ " " ++ show x ++ " " ++ parens (show r)

fromList :: (Setlike f, Ord a) => [a] -> f a
fromList [] = emp
fromList (x:xs) = insert x (fromList xs)

difference :: (Setlike f, Ord a) => f a -> f a -> f a
difference xs ys = fold (\x acc -> if (x `isIn` ys) then delete x acc else acc) xs xs

subset :: (Setlike f, Ord a) => f a -> f a -> Bool
subset xs ys = fold (\x acc -> x `isIn` ys && acc) True xs

newtype KV k v = KV { kv :: (k,v) }

instance Eq k => Eq (KV k v) where
  (KV kv1) == (KV kv2) = fst kv1 == fst kv2

instance Ord k => Ord (KV k v) where
  compare (KV kv1) (KV kv2) = compare (fst kv1) (fst kv2)

instance (Show k, Show v) => Show (KV k v) where
  show (KV (k,v)) = show k ++ " |-> " ++ show v

type Map f k v = f (KV k v)
type ListMap k v = Map [] k v
type TreeMap k v = Map BST k v

emptyMap :: Setlike f => Map f k v
emptyMap = emp

--getElem :: Ord a => a -> f a -> Maybe a

lookup :: (Setlike f, Ord k) => k -> Map f k v -> Maybe v
lookup k m = case (getElem (KV (k, undefined)) m) of
             Nothing -> Nothing
             Just (KV (k, v)) -> Just v


extend :: (Setlike f, Ord k) => k -> v -> Map f k v -> Map f k v
extend k v m = insert kv m
  where
    kv = KV (k,v)

 -- delete :: Ord a => a -> f a -> f a
remove :: (Setlike f, Ord k) => k -> Map f k v -> Map f k v
remove k m = delete (KV (k, undefined)) m 

toAssocList :: Setlike f => Map f k v -> [(k,v)]
toAssocList = fold (\x -> (:) (f x)) []
    where
      f (KV (k,v)) = (k,v)

data RoseTree a = Leaf a | Branch [RoseTree a] deriving (Eq, Show)

instance Functor RoseTree where
  --fmap:: (a -> b) -> RoseTree a -> RoseTree b
  fmap f (Leaf a) = (Leaf (f a))
  fmap f (Branch a) = Branch (map (fmap f) a)

-- data BST a = Empty | Node (BST a) a (BST a)
instance Functor BST where
  --fmap::(a->b) -> BST a -> BST b
  fmap _ Empty = Empty
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

{-
Buggy behaviour when we apply something like 'even' with fmap to a BST
fmap is supposed to preserve structure, but booleans have no natural ordering,
(although Haskell orders booleans False < True)
which is an important element of a BST. More generally, if we map any
function that changes the ordering of elements, such as (\x -> 100-x)
-}

