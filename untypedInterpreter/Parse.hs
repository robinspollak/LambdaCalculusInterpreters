module Parse where

import Control.Monad()
import Control.Monad.IO.Class()
import Control.Applicative

import Data.Array.IO()

import Data
import ParseBoilerplate

parseL :: Parser LC
parseL = term

term = parseLet
       <|> foldl1 App <$> some factor 
       <|> factor

factor = genLambdas <$ kw "lambda" <*> some var <* char '.' <*> term
         <|> atom

genLambdas :: [VarName] -> LC -> LC
genLambdas [] _      = error "how'd u get here, fam?"
genLambdas [x] lc    = Lambda x lc
genLambdas (x:xs) lc = Lambda x (genLambdas xs lc)

atom = Var <$> var <|> Num <$> num <|> parens term

parseLet :: Parser LC
parseLet = Let <$ kw "let" <*> var <* str "=" <*> term <* kw "in" <*> term

lcparser :: String -> Either Error LC
lcparser s = case parse parseL s of
             Just (x, "")  -> Right x
             Just (_, str) -> Left $ ParseError str
             Nothing       -> Left $ ParseError s
