module Parse where

import Control.Monad()
import Control.Monad.IO.Class()
import Control.Applicative

import Data.Array.IO()
import Data.Char

import Data

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
    empty = Parser $ \_ -> Nothing
    l <|> r = Parser $ \s -> parse l s <|> parse r s

ensure :: (a -> Bool) -> Parser a -> Parser a
ensure p parser = Parser $ \s ->
   case parse parser s of
     Nothing -> Nothing
     Just (a,s') -> if p a then Just (a,s') else Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where f [] = Nothing
        f (x:xs) = if p x then Just (x,xs) else Nothing

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
keywords = ["lambda", "let", "in", "bool", "int", "->", "true", "false", "if", "then", "else", "not", "and", "or", "fst", "snd", "rec"]

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

parseL :: Parser LC
parseL = term

term = parseLetRec
       <|> parseLet
       <|> foldl1 App <$> some factor
       <|> If <$ kw "if" <*> term <* kw "then" <*> term <* kw "else" <*> term
       <|> factor

factor = (\arg term -> Lambda (fst arg) (snd arg) term) <$ kw "lambda" <*> parseVarTypePair <* char '.' <*> term
         <|> genLambdas <$ kw "lambda" <*> some (parens parseVarTypePair) <* char '.' <*> term
         <|> parseBinOp
         <|> atom

atom = UnOpLC <$> parseUnOp <*> atom
       <|> Var <$> var
       <|> parseAscr
       <|> Num <$> num
       <|> parseBool
       <|> parsePair
       <|> parens term

parseLetRec :: Parser LC
parseLetRec = LetRec <$ kw "let" <* kw "rec" <*> var <* char ':' <*> parseType <* char '=' <*> term <* kw "in" <*> term

parseType :: Parser Type
parseType = TFunction <$> parseTypeAtom <* str "->" <*> parseType
            <|> parseTypeAtom

parseTypeAtom = TBool <$ kw "bool"
                <|> TInt <$ kw "int"
                <|> TPair <$ char '(' <*> parseType <* char ',' <*> parseType <* char ')'
                <|> parens parseType


parseVarTypePair :: Parser (VarName, Type)
parseVarTypePair = (\name typ -> (name, typ)) <$> var <* char ':' <*> parseType 


parseBinOp :: Parser LC
parseBinOp = chainl1 atom parseBinOp'

parseBinOp' :: Parser (LC -> LC -> LC)
parseBinOp' = BinOpLC And <$ str "and"
              <|> BinOpLC Times <$ str "*"
              <|> BinOpLC Div <$ str "/"
              <|> BinOpLC Or <$ kw "or"
              <|> BinOpLC Plus <$ str "+"
              <|> BinOpLC Minus <$ str "-"
              <|> BinOpLC Equ <$ str "=="
              <|> neqHelp <$ str "/="
              <|> gtEqHelp <$ str ">="
              <|> ltEqHelp <$ str "<="
              <|> BinOpLC Lt <$ str "<"
              <|> gtHelp <$ str ">"


gtHelp, gtEqHelp, ltEqHelp, neqHelp :: LC -> LC -> LC
gtHelp first second = BinOpLC And (UnOpLC Not (BinOpLC Lt first second)) (UnOpLC Not (BinOpLC Equ first second))

gtEqHelp first second = UnOpLC Not (BinOpLC Lt first second)

ltEqHelp first second = BinOpLC Or (BinOpLC Lt first second) (BinOpLC Equ first second)

neqHelp first second = UnOpLC Not (BinOpLC Equ first second)

genLambdas :: [(VarName, Type)] -> LC -> LC
genLambdas [] _      = error "how'd u get here, fam?"
genLambdas [x] lc    = Lambda (fst x) (snd x) lc
genLambdas (x:xs) lc = Lambda (fst x) (snd x) (genLambdas xs lc)


parseAscr :: Parser LC
parseAscr = Ascr <$ char '(' <*> term <* char ':' <*> parseType <* char ')'

parsePair :: Parser LC
parsePair = Pair <$ char '(' <*> term <* char ',' <*> term <* char ')'

parseUnOp :: Parser UnOp
parseUnOp = Not <$ kw "not"
            <|> Neg <$ str "-"
            <|> Fst <$ kw "fst"
            <|> Snd <$ kw "snd"

parseBool :: Parser LC
parseBool = LCBool <$ kw "true" <*> pure True
            <|> LCBool <$ kw "false" <*> pure False

parseLet :: Parser LC
parseLet = Let <$ kw "let" <*> var <* str "=" <*> term <* kw "in" <*> term
           <|> (\v t lc1 lc2 -> Let v (Ascr lc1 t) lc2) <$ kw "let" <*> var <* char ':' <*> parseType <* char '=' <*> term <* kw "in" <*> term
