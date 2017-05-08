module ParseBoilerplate where

import Control.Monad()
import Control.Monad.IO.Class()
import Control.Applicative

import Data.Array.IO()
import Data.Char

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
