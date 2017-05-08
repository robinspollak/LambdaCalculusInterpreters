module Parse where

import Control.Monad()
import Control.Monad.IO.Class()
import Control.Applicative

import Data.Array.IO()

import Data
import ParseBoilerplate

parseL :: Parser LC
parseL = topLevel

topLevel = parseLetRec
           <|> parseLet 
           <|> chainl1 secondLevel (BinOpLC Or <$ kw "or")
           <|> secondLevel

secondLevel = chainl1 thirdLevel (BinOpLC And <$ kw "and")
              <|> thirdLevel

thirdLevel = chainl1 fourthLevel parseComparator
             <|> fourthLevel

fourthLevel = chainl1 fifthLevel parsePlusMinus
              <|> fifthLevel

fifthLevel = chainl1 sixthLevel parseTimesDiv
             <|> sixthLevel

sixthLevel = UnOpLC <$> parseUnOp <*> atom
             <|> foldl1 App <$> some atom
             <|> atom

atom  = parseLambdas
        <|> parseIf
        <|> parseAscr
        <|> Var <$> var
        <|> Num <$> num
        <|> parseBool
        <|> parsePair
        <|> parens topLevel

parseLetRec :: Parser LC
parseLetRec = LetRec <$ kw "let" <* kw "rec" <*> var <* char ':' <*> parseType <* char '=' <*> topLevel <* kw "in" <*> topLevel

parseType :: Parser Type
parseType = TFunction <$> parseTypeAtom <* str "->" <*> parseType
            <|> parseTypeAtom

parseTypeAtom = TBool <$ kw "bool"
                <|> TInt <$ kw "int"
                <|> TPair <$ char '(' <*> parseType <* char ',' <*> parseType <* char ')'
                <|> parens parseType


parseVarTypePair :: Parser (VarName, Type)
parseVarTypePair = (\name typ -> (name, typ)) <$> var <* char ':' <*> parseType 

parseComparator :: Parser (LC -> LC -> LC)
parseComparator = BinOpLC Equ <$ str "=="
                  <|> neqHelp <$ str "/="
                  <|> gtEqHelp <$ str ">="
                  <|> ltEqHelp <$ str "<="
                  <|> BinOpLC Lt <$ str "<"
                  <|> gtHelp <$ str ">"

parsePlusMinus :: Parser (LC -> LC -> LC)
parsePlusMinus = BinOpLC Plus <$ str "+"
                 <|> BinOpLC Minus <$ str "-"

parseTimesDiv :: Parser (LC -> LC -> LC)
parseTimesDiv = BinOpLC Times <$ str "*"
                <|> BinOpLC Div <$ str "/"

gtHelp, gtEqHelp, ltEqHelp, neqHelp :: LC -> LC -> LC
gtHelp first second = BinOpLC And (UnOpLC Not (BinOpLC Lt first second)) (UnOpLC Not (BinOpLC Equ first second))
gtEqHelp first second = UnOpLC Not (BinOpLC Lt first second)
ltEqHelp first second = BinOpLC Or (BinOpLC Lt first second) (BinOpLC Equ first second)
neqHelp first second = UnOpLC Not (BinOpLC Equ first second)

parseIf = If <$ kw "if" <*> topLevel <* kw "then" <*> topLevel <* kw "else" <*> topLevel

parseLambdas = (\arg contents -> Lambda (fst arg) (snd arg) contents) <$ kw "lambda" <*> parseVarTypePair <* char '.' <*> topLevel
               <|> genLambdas <$ kw "lambda" <*> some (parens parseVarTypePair) <* char '.' <*> topLevel

genLambdas :: [(VarName, Type)] -> LC -> LC
genLambdas [] _      = error "how'd u get here, fam?"
genLambdas [x] lc    = Lambda (fst x) (snd x) lc
genLambdas (x:xs) lc = Lambda (fst x) (snd x) (genLambdas xs lc)

parseAscr :: Parser LC
parseAscr = Ascr <$ char '(' <*> topLevel <* char ':' <*> parseType <* char ')'

parsePair :: Parser LC
parsePair = Pair <$ char '(' <*> topLevel <* char ',' <*> topLevel <* char ')'

parseUnOp :: Parser UnOp
parseUnOp = Not <$ kw "not"
            <|> Neg <$ str "-"
            <|> Fst <$ kw "fst"
            <|> Snd <$ kw "snd"

parseBool :: Parser LC
parseBool = LCBool <$ kw "true" <*> pure True
            <|> LCBool <$ kw "false" <*> pure False

parseLet :: Parser LC
parseLet = Let <$ kw "let" <*> var <* str "=" <*> topLevel <* kw "in" <*> topLevel
           <|> (\v t lc1 lc2 -> Let v (Ascr lc1 t) lc2) <$ kw "let" <*> var <* char ':' <*> parseType <* char '=' <*> topLevel <* kw "in" <*> topLevel
