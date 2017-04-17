module Hw06 where

import Control.Monad
import Control.Applicative

import Data.Array.IO
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map(..),(!))
import Data.Maybe ( fromMaybe )

import System.Environment
import System.Exit
import System.IO
import System.Random
import System.Console.GetOpt

import Test.QuickCheck
import Test.QuickCheck.Monadic

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

instance Functor Parser where
    fmap f p = Parser $ \s -> (\(a,c) -> (f a, c)) <$> parse p s

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a,s)
    f <*> a = Parser $ \s ->
        case parse f s of
            Just (g, s') -> parse (fmap g a) s'
            Nothing -> Nothing

instance Alternative Parser where
    empty = Parser $ \s -> Nothing
    l <|> r = Parser $ \s -> parse l s <|> parse r s

ensure :: (a -> Bool) -> Parser a -> Parser a
ensure p parser = Parser $ \s ->
   case parse parser s of
     Nothing -> Nothing
     Just (a,s') -> if p a then Just (a,s') else Nothing

lookahead :: Parser (Maybe Char)
lookahead = Parser f
  where f [] = Just (Nothing,[])
        f (c:s) = Just (Just c,c:s)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (x:xs) = if p x then Just (x,xs) else Nothing

eof :: Parser ()
eof = Parser $ \s -> if null s then Just ((),[]) else Nothing

ws :: Parser ()
ws = pure () <* many (satisfy isSpace)

char :: Char -> Parser Char
char c = ws *> satisfy (==c)

str :: String -> Parser String
str s = ws *> loop s
  where loop [] = pure []
        loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs

parens :: Parser a -> Parser a
parens p = (char '(' *> p) <* char ')'

keywords :: [String]
keywords = ["lambda", "let", "in"]

isKeyword = (`elem` keywords)

kw :: String -> Parser String
kw s = Parser $ \t ->
      case parse (str s) t of
        Just (s, x:xs) | not (isAlphaNum x) -> Just (s, x:xs)
        Just (s, "")                        -> Just (s, "")
        _                                   -> Nothing

var :: Parser String
var = ensure (not . isKeyword) $ (++) <$ ws <*> some (satisfy isAlpha) <*> many (satisfy (\a -> (isAlphaNum a || a == (toEnum 39))))

num :: Parser Int
num = ws *> (read <$> some (satisfy isDigit))

spaces :: Parser ()
spaces = many (satisfy isSpace) *> pure ()

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p sep = foldl (\acc (op,v) -> op acc v) <$>
               p <*> many ((\op v -> (op,v)) <$> sep <*> p)

type VarName = String

data LC = Var VarName | App LC LC | Lambda VarName LC | Let VarName LC LC | Num Int

instance Show LC where
  show = showTerm

parens' :: String -> String
parens' x = "(" ++ x ++ ")"

showTerm :: LC -> [Char]
showTerm (App x y) = (showTerm x) ++ " " ++ (showFactor y)
showTerm (Let v x y) = "let " ++ v ++ " = " ++ showTerm x ++ " in " ++ showTerm y
showTerm x = showFactor x

showLambda :: LC -> [Char]
showLambda (Lambda v expr) = case expr of
                             e@(Lambda v' expr) -> v ++ " " ++ showLambda e
                             x                  -> v ++ ". " ++ showTerm x

showFactor :: LC -> [Char]
showFactor e@(Lambda v expr) = "lambda " ++ showLambda e
showFactor x = showAtom x

showAtom :: LC -> [Char]
showAtom (Var x) = x
showAtom (Num n) = show n
showAtom x = parens' $ showTerm x

parseL :: Parser LC
parseL = term

term = parseLet
       <|> foldl1 App <$> some factor 
       <|> factor

factor = genLambdas <$ kw "lambda" <*> some var <* char '.' <*> term
         <|> atom


genLambdas :: [VarName] -> LC -> LC
genLambdas [x] lc = Lambda x lc
genLambdas (x:xs) lc = Lambda x (genLambdas xs lc)

atom = Var <$> var <|> Num <$> num <|> parens term

parseLet :: Parser LC
parseLet = Let <$ kw "let" <*> var <* str "=" <*> term <* kw "in" <*> term

data Error = ParseError String | AppliedNonFunction LC | UnboundVariables [VarName] | BadChurch LC

lcparser :: String -> Either Error LC
lcparser s = case parse parseL s of
             Just (x, "")  -> Right x
             Just (x, str) -> Left $ ParseError str
             Nothing       -> Left $ ParseError s

interp :: LC -> Either Error LC
interp ast = (evalCBN ast) >>= simplify

evalCBN :: LC -> Either Error LC
evalCBN (Var x) = Right $ Var x
evalCBN (Lambda x e) = Right $ Lambda x e
evalCBN (Let v e1 e2) = evalCBN (subst e2 v e1)
evalCBN (App e1 e2) = case evalCBN e1 of
                       (Right (Lambda x e')) -> evalCBN (subst e' x e2)
                       (Left e)              -> Left e
                       (Right x)             -> Left $ AppliedNonFunction x

simplify :: LC -> Either Error LC
simplify (Var x) = Right $ Var x
simplify (Lambda x e) = do
                        exp <- simplify e
                        Right $ (Lambda x) exp
simplify (Let v e1 e2) = simplify (subst e2 v e1)
simplify (App e1 e2) =  case evalCBN e1 of
                        (Right (Lambda x e')) -> do
                                                 exp2 <- simplify e2
                                                 simplify (subst e' x exp2)
                        (Left e)              -> Left e
                        (Right exp)           -> do
                                                 exp1 <- simplify e1
                                                 exp2 <- simplify e2
                                                 Right $ App exp1 exp2

subst :: LC -> VarName -> LC -> LC
subst (Var e) v sub | e == v    = sub
                    | otherwise = Var e
subst (App l1 l2) v sub         = App (subst l1 v sub) (subst l2 v sub)
subst e@(Lambda v1 l) v2 sub | v1 == v2 = e
                             | otherwise = Lambda v1 $ subst l v2 sub
subst (Let v1 l1 l2) v2 sub     = Let v1 (subst l1 v2 sub) (subst l2 v2 sub)

convertChurch :: LC -> Either Error Int
convertChurch (Var x) = 0
convertChurch e@(Lambda x (Lambda y (Var z))) | x==z = Left $ BadChurch e
convertChurch (Lambda x (Lambda y body)) = convertChurch body
convertChurch (App (Var x) body) = 1 + convertChurch body
convertChurch e  = Left $ BadChurch e

convertInt :: Int -> LC
convertInt n = Lambda "s" (Lambda "z" (convertHelp n))
           where convertHelp 0 = Var "z"
                 convertHelp n = App (Var "s") $ convertHelp (n - 1)

convertInts :: LC -> Either Error LC
convertInts (Num n) = Right $ convertInt n
convertInts (Var x) = Right $ Var x
convertInts (Lambda v e) = do
                           exp <- convertInts e
                           Right $ Lambda v exp
convertInts (App e1 e2) = do
                          exp1 <- convertInts e1
                          exp2 <- convertInts e2
                          Right $ App exp1 exp2
convertInts (Let v e1 e2) = do
                            exp1 <- convertInts e1
                            exp2 <- convertInts e2
                            Right $ Let v exp1 exp2

bv :: LC -> (Set VarName)
bv (Var x) = Set.empty
bv (Lambda x e) = Set.union (Set.singleton x) (bv e)
bv (Let l e1 e2) = Set.union (bv e1) (bv e2)
bv (App e1 e2) = Set.union (bv e1) (bv e2)

allVars :: LC -> (Set VarName)
allVars (Var x) = Set.singleton x
allVars (Lambda x e) = Set.union (Set.singleton x) (allVars e)
allVars (Let l e1 e2) = Set.difference (Set.union (allVars e1) (allVars e2)) (Set.singleton l)
allVars (App e1 e2) = Set.union (allVars e1) (allVars e2)

ubv :: LC -> (Set VarName)
ubv lc = Set.difference (allVars lc) (bv lc)

data Flag = Help | Numeral | Check deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
            Option "?" [] (NoArg Help) "help message",
            Option "n" [] (NoArg Numeral) "church numeral conversion",
            Option "c" [] (NoArg Check) "check for unbound variables"
          ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = case getOpt Permute options argv of
                      (o,n,[]) -> return (o,n)
                      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
                    where header = "Usage: ic [OPTION...] file"

helpMessage = "interp [OPTIONS] FILE (defaults to -, for stdin) \n lambda calculus interpreter \n\n -c --check    Check scope \n -n --numeral  Convert final Church numeral to a number \n -? --help     Display help message"

-- main :: IO ()
-- main = do
--        args <- getArgs
--        opts <- compilerOpts args
--        if (Help `elem` fst opts) then putStrLn helpMessage else case opts of
--                     (flags, [])    -> getContents >>= (handleStdIn flags) >>= putStrLn
--                     (flags, ["-"]) -> getContents >>= (handleStdIn flags) >>= putStrLn
--                     (flags, [fp])  -> ((interpFile flags fp) >>= putStrLn)
--                     _              -> fail "Please provide a file or data on stdin for us to interpret"

-- interpFile :: [Flag] -> String -> Either Error String
-- interpFile flags fp = do
--                       file <- readFile fp
--                       let parsed = intermediateHandling flags file
--                       interped <- interp parsed
--                       let interped' = if Numeral `elem` flags then show (convertChurch (lcparser interped)) else interped
--                       return interped'

handleStdIn :: [Flag] -> String -> Either Error LC
handleStdIn flags inp = do
                        parsed   <- intermediateHandling flags inp
                        interped <- interp parsed
                        interped'<- if Numeral `elem` flags
                                    then convertChurch interped
                                    else interped
                        return interped'


intermediateHandling :: [Flag] -> String -> Either Error LC
intermediateHandling flags inp = do
                                 let input = intercalate "\n" (filter (\x -> x/="") (splitOn "\n" inp))
                                 parsed  <- lcparser input
                                 numeral <- if Numeral `elem` flags then convertInts parsed else return parsed
                                 check   <- if Check `elem` flags && (ubv numeral /= Set.empty)
                                            then Left $ (UnboundVariables (Set.toList (ubv numeral)))
                                            else return numeral
                                 return check