Homework 4.0: Parsing
Due 2017-03-05

> {-# OPTIONS_GHC -Wall -fno-warn-unused-imports #-}

> module Hw04 where
>
> import Control.Applicative
> import Data.Char
>
> import qualified Data.Map as Map
> import Data.Map (Map)

In this homework, you'll write some pretty-printers and parsers for
the While language of HW03.

> type VarName = String
>
> data AExp =
>     Var VarName
>   | Num Int
>   | Plus AExp AExp
>   | Times AExp AExp
>   | Neg AExp
>   | Div AExp AExp
>   deriving Eq
>
> data BExp =
>     Bool Bool
>   | Equal AExp AExp
>   | Lt AExp AExp
>   | Not BExp
>   | Or BExp BExp
>   | And BExp BExp
>   deriving (Show, Eq)
>
> data Stmt =
>     Skip
>   | Assign VarName AExp
>   | Seq Stmt Stmt
>   | If BExp Stmt Stmt
>   | While BExp Stmt
>   deriving (Show, Eq)


<h3>Problem 1: Pretty printing with `Show`</h3>

Write a `Show` instance for `AExp` that prints as few parentheses as
possible. That is, `show (Plus (Var "x") (Plus (Num 6) (Num 36)))`
should return `"x + 6 + 36"`, not `"x + (6 + 36)"`. Similarly, `show
(Plus (Num 5) (Times (Num 6) (Num 36)))` should return `5 + 6 * 36`,
because multiplication has a higher precedence than addition. But
`show (Times (Num 5) (Plus (Num 6) (Num 36)))` should return `"5 * (6
+ 36)"`. Finally, you should encode *syntactic sugar* in the pretty
printing: if you encounter a node like `Plus (Num 5) (Neg (Var x))`,
please render it as `"5 - x"`. Take care that subtraction and division
are both left associative.

The trick is to have the pretty printer follow the structure of the
parser. There are two approaches: either take an extra integer
parameter and use it to determine what "level" you're at, or actually
write several functions (`showTerm`, `showFactor`, etc.).


> instance Show AExp where
>   show (Var var) = var
>   show (Num x) = show x
>   show (Plus x (Neg y)) = showTerm x ++ " - " ++ showTerm y
>   show (Plus x y) = showTerm x ++ " + " ++ showTerm y
>   show (Times x y) = showFactor x ++ " * " ++ showFactor y
>   show (Neg x) = "-" ++ show x
>   show (Div x y) = showFactor x ++ " / " ++ showFactor y
>
>
> parens' :: [Char] -> [Char]
> parens' x = "(" ++ x ++ ")"
>
>
> showTerm :: AExp -> [Char]
> showTerm (Times x y) = parens' $ showFactor x ++ " * " ++ showFactor y
> showTerm (Div x y) = parens' $ showFactor x ++ " * " ++ showFactor y
> showTerm (Plus x (Neg y)) = parens' $ showTerm x ++ " - " ++ showTerm y
> showTerm x = show x
>
> showFactor :: AExp -> [Char]
> showFactor (Plus x (Neg y)) = parens' $ showTerm x ++ " - " ++ showTerm y
> showFactor (Plus x y) = parens' $ showTerm x ++ " + " ++ showTerm y
> showFactor x = show x
>
> fivethreetwo = Plus (Num 5) (Plus (Neg (Num 3)) (Neg (Num 2)))
> garbage = Div (Times (Num 2) (Plus (Times (Num 5) (Div (Num 4) (Num 3))) (Times (Neg (Num 3)) (Num 4)))) (Num 10)


<h3>Problem 2: Parsing expressions by hand</h3>

Traditionally, parsing is broken into two phases: *lexing*, which
converts a string to a list of *tokesn*; and *parsing*, which converts
a list of tokens to an abstract syntax tree (AST). In this problem,
you'll extend a lexer and parser written for arithmetic expressions to
one that lexes and parses a version of the While language above.

To start with, we'll define some tokens, the core "meaningful" bits of
our language. Consider the concrete string `2 + 10 * 3`. The relevant
tokens are `2`, `+`, `10`, `*`, and `3`. Note that I left whitespace
out: I expect `2 + 10 * 3` to behave the same as `2+10*3`---most
languages ignore whitespace (but just wait until Problem 4!). Here's a
data definition for the relevant kind of tokens:

> data Token =
>     TNum Int
>   | TId String
>   | TPlus
>   | TMinus
>   | TTimes
>   | TDiv
>   | TLParen
>   | TRParen


>   deriving (Show, Eq)


> lexer :: String -> [Token]
> lexer [] = []
> lexer (w:s) | isSpace w = lexer (dropWhile isSpace s)
> lexer ('+':s) = TPlus:lexer s
> lexer ('-':s) = TMinus:lexer s
> lexer ('*':s) = TTimes:lexer s
> lexer ('/':s) = TDiv:lexer s
> lexer ('(':s) = TLParen:lexer s
> lexer (')':s) = TRParen:lexer s


> lexer s | isAlpha (head s) =
>   let (id,s') = span isAlphaNum s in
>   TId id:lexer s'
> lexer s | isDigit (head s) =
>   let (n,s') = span isDigit s in
>   TNum (read n :: Int):lexer s'
> lexer (n:_) = error $ "Lexer error: unexpected character " ++ [n]

Test this lexer out, and convince yourself it correctly translates a
string into its constituent tokens. Concretely, the string `"2 + 10 *
3"` should produce the token list `[TNum 2, TPlus, TNum 10, TTimes,
TNum 3]`. The lexer recurs over its input, identifying which token is
at the front each time. Note how it's careful to dispose of whitespace
first, then check for appropriate symbols, then convert to numbers,
and finally to give up.

Parsers operate on streams of tokens, trying to produce an AST; each
parser function also returns the remaining stream of tokens. Take
particular care to note the loops in play: `parseATerm`: calls
`parseATerm'`, which is mutually recursive with `parseATerm''`; there's
a similar setup with `parseAFactor`, `parseAFactor'`, and
`parseAFactor''`.

> parseATerm :: [Token] -> Either String (AExp,[Token])
> parseATerm ts = 
>   case parseAFactor ts of
>     Right (f,ts) -> parseATerm' f ts
>     Left e -> Left e
>       
> parseATerm' :: AExp -> [Token] -> Either String (AExp,[Token])
> parseATerm' lhs [] = Right (lhs, [])
> parseATerm' lhs (TPlus:ts) = parseATerm'' (Plus lhs) ts
> parseATerm' lhs (TMinus:ts) = parseATerm'' (Plus lhs . Neg) ts
> parseATerm' lhs ts = parseAFactor' lhs ts

Notice how we just encoded subtraction above! It's just like what we
did in HW02 with the `translate` function, but we've built it into our
parser. Adding niceties like this is called *syntactic sugar*.

> parseATerm'' :: (AExp -> AExp) -> [Token] -> Either String (AExp,[Token])
> parseATerm'' mk [] = Left $ "expected term after +/-"
> parseATerm'' mk ts = 
>   case parseAFactor ts of
>     Right (e,ts) -> parseATerm' (mk e) ts
>     Left e -> Left e
> 
> parseAFactor :: [Token] -> Either String (AExp,[Token])
> parseAFactor ts =
>   case parseAAtom ts of
>     Right (lhs,ts) -> parseAFactor' lhs ts
>     Left e -> Left e
>     
> parseAFactor' :: AExp -> [Token] -> Either String (AExp,[Token])
> parseAFactor' lhs (TTimes:ts) = parseAFactor'' (Times lhs) ts
> parseAFactor' lhs (TDiv:ts) = parseAFactor'' (Div lhs) ts
> parseAFactor' lhs ts = Right (lhs, ts)
> 
> parseAFactor'' :: (AExp -> AExp) -> [Token] -> Either String (AExp,[Token])
> parseAFactor'' mk [] = Left $ "expected term after *"
> parseAFactor'' mk ts = 
>   case parseAAtom ts of
>     Right (e,ts) -> parseAFactor' (mk e) ts
>     Left e -> Left e
> 
> parseAAtom :: [Token] -> Either String (AExp, [Token])
> parseAAtom (TNum n:ts) = Right (Num n, ts)
> parseAAtom (TId id:ts) = Right (Var id, ts)
> parseAAtom (TLParen:ts) =
>   case parseATerm ts of
>     Right (e,TRParen:ts') -> Right (e,ts')
>     Right (_,ts) -> Left $ "expected right paren, found: " ++ show ts
>     Left e -> Left e
> parseAAtom ts = Left $ "expected numbers or parens, found: " ++ show ts

To test this parser, play with the following "composed" function that
combines the lexer and parser into one:

> tryParse :: ([Token] -> Either String (a,[Token])) -> String -> a
> tryParse parser s =
>   case parser $ lexer s of
>     Right (e,[]) -> e
>     Right (_,ts) -> error $ "Parse error: expected EOF, found: " ++ show ts
>     Left e -> error $ "Parser error: " ++ e
>
> tryParseATerm :: String -> AExp
> tryParseATerm = tryParse parseATerm

Your job is to finish writing a parser for the While language in an
old school Pascal-style syntax. Here are some example programs:

> pascalProg1 = "x := 5; y := 6"
> pascalProg2 = "IF x = 0 THEN y := 10 ELSE y := x END"
> pascalProg3 = "IF NOT (x = 0) THEN y := 10 ELSE y := x END"
> pascalProg4 = "IF x != 0 THEN y := 10; SKIP ELSE y := x END"
> pascalProg5 = " \t\nWHILE IFFY > 10 AND (true OR false) DO IFFY := IFFY - 1 END;\n\nderp := 7"
> pascalProg6 = "WHILE true DO SKIP END"
> pascalProg7 = "IF x>=0 THEN y := 10 ELSE SKIP END;\ny := x + y;\nx := 5"
> pascalProg8 = "IF x=46 THEN x := x + 1 END; oneArm := x * 2"

Note that some of these syntactic forms aren't *directly* supported in
the AST (e.g., there's no way to directly say `>=` in `BExp`); you'll
have to encode these ideas using *syntactic sugar*. Also, be careful:
`IF` is a keyword, but `IFFY` is a variable name.

These syntactic forms *should* cover all of the corner cases, but I
might have missed one. On Monday, February 27th, please bring a couple
of example programs to class so we can discuss them.


You'll need to:

  1. Extend the `Token` type to account for other tokens (e.g., `WHILE`).
  2. Change the lexer to find these new tokens. (Watch out for keyword/variable 
     collisions!)
  3. Write a parser for boolean expressions. If you're wondering about
     precedence, think of `OR` like `+` and `AND` like `*`. For relations
     like `=` and `!=` and `>=`, they should be parsed under OR, e.g.,
     `"x = y OR x < y AND y = z"` should parse as 
     `And (Or (Equal (Var "x") (Var "y")) (Lt (Var "x") (Var "y"))) (Equal (Var "y") (Var "z"))`.
  4. Write a parser for commands/statements.
                
You should *largely* be able to follow the pattern for arithmetic
expressions established already, but you'll have to be creative in a
few spots.

Parsers are hard to write by hand. The surest way to avoid bugs is to write clean code.



I will *only* test your last step, the `pascalSyntax` function. I've
given you a bit of a start here: I suggest you split statement parsing
in two parts: one part for parsing a sequence of statements and one
part for parsing an individual stement.

> parseStmts :: [Token] -> Either String (Stmt,[Token])
> parseStmts ts = undefined
>
> pascalSyntax :: String -> Stmt
> pascalSyntax = tryParse parseStmts

Do *not* change `pascalSyntax`'s type or general behavior---whatever
you do, your parser should follow the general scheme of
the `tryParse` functions we've seen.

<h3>Problem 3: Parsing expressions using Applicative parser combinators</h3>

Problem 3 is long enough that I've marked the things you should do
with **HWHWHW**. I haven't done so for the other problems, which have
fewer parts.

A `Parser a` is a function that takes a `String` and may return an `a`
and a `String`. Intuitively, a Parser either (1) fails to parse, or
(2) parses part of the input (at type `a`) and has some remaining,
unparsed input.

> newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

We'll start out by defining a few necessary instances, but our goal
isn't to write `Parser (\s -> ...)` by hand: we'll construct a library
of *combinators* where we can build up parsers nicely.

But first, `Parser` is a functor: `fmap` applies to the parsed value.

> instance Functor Parser where
>   fmap f p = Parser $ \s -> (\(a,c) -> (f a, c)) <$> parse p s

`Parser` is also applicative: a pure `Parser` always succeeds, just
returning its input. The `(<*>)` (read: "ap") function takes an `f ::
Parser (a -> b)` and a `Parser a` and combines them: it first parses
`f` to get a function `g :: a -> b` and some remaining bit of string
to be parsed, `s'`. It then parses `s'` with `a :: Parser a`, making
sure to apply `g` to the result. (Note how we use `fmap` to concisely
say "apply `g` to the result of parsing with `a`.)

> instance Applicative Parser where
>   pure a = Parser $ \s -> Just (a,s)
>   f <*> a = Parser $ \s ->
>     case parse f s of
>       Just (g,s') -> parse (fmap g a) s'
>       Nothing -> Nothing

There's one last type class to define: `Alternative`, which is a
left-biased choice. In the `empty` case, we just return
`Nothing`. When considering two alternatives, we can take advantage of
the existing `Alternative` class for `Maybe`: try to parse the
left-hand side, and if that fails, try the right.

> instance Alternative Parser where
>   empty = Parser $ \s -> Nothing
>   l <|> r = Parser $ \s -> parse l s <|> parse r s

Below we'll define a few "primitive" parsers, which explicitly write
parsing functions. Don't define any extra ones! All of your solutions
should use (a) these primitive parsers, (b)
Functor/Applicative/Alternative, and (c) other helpers/recursive
functions you define.

> ensure :: (a -> Bool) -> Parser a -> Parser a
> ensure p parser = Parser $ \s ->
>    case parse parser s of
>      Nothing -> Nothing
>      Just (a,s') -> if p a then Just (a,s') else Nothing
>
> lookahead :: Parser (Maybe Char)
> lookahead = Parser f
>   where f [] = Just (Nothing,[])
>         f (c:s) = Just (Just c,c:s)
>
> satisfy :: (Char -> Bool) -> Parser Char
> satisfy p = Parser f
>   where f [] = Nothing
>         f (x:xs) = if p x then Just (x,xs) else Nothing

We can detect the end of the file manually...

> eof :: Parser ()
> eof = Parser $ \s -> if null s then Just ((),[]) else Nothing

...but the other three primitive parsers are enough on their
own.

**HWHWHW** Write a function `eof'` which behaves like `eof` but is built up
from the other primitive parsers:

> eof' :: Parser ()
> eof' = undefined

In general, we'll be careful to allow arbitrary whitespace at the
beginnings of what we parse.

> ws :: Parser ()
> ws = pure () <* many (satisfy isSpace)
>
> char :: Char -> Parser Char
> char c = ws *> satisfy (==c)
>
> str :: String -> Parser String
> str s = ws *> loop s
>   where loop [] = pure []
>         loop (c:cs) = (:) <$> satisfy (==c) <*> loop cs

We can define what it means to parse something in parentheses. Note
how we're careful to specify order of operations between `(*>)` and
`(<*)`---forgetting to do this can lead to some tricky bugs!

> parens :: Parser a -> Parser a
> parens p = (char '(' *> p) <* char ')'

Let's start our parser from the bottom up. We'll begin with a notion
of *keyword* a/k/a a *reserved word*. These are English words that
can't be used as variables because they'll be part of the concrete
syntax of our language.

> keywords :: [String]
> keywords = ["skip","if","then","else","while","true","false"]
>
> isKeyword = (`elem` keywords)

To parse a keyword, we'll parse it as a string (using `str`) but make
sure that it's the whole identifier. For example, `parse (kw "repeat")
"repeat "` should return `Just ("repeat"," ")`, but `parse (kw
"repeat") "repeatable"` should return `Nothing`.

**HWHWHW** Write the that parses a keyword (you don't need to check
that the given string is in the list `keywords`).

> kw :: String -> Parser String
> kw s = undefined


**HWHWHW** Now let's parse variables. A variable is (1) a string
starting with an alphabetical character followed by zero or more
alphanumeric characters that (2) isn't a keyword.

> var :: Parser String
> var = undefined


A number is one or more digits. We can use `read :: Read a => String
-> a` to read out an Int, since a `Read Int` instance is defined in
the Prelude. Note that the type annotation here is critical, or
Haskell will complain---it doesn't know what you want to read!

> num :: Parser Int
> num = ws *> (read <$> some (satisfy isDigit))

**HWHWHW** Now define an `AExp` parser; call the top-level "terms"
`aexp`, but you can call the rest whatever you want. Feel free to
copy/paste most of it from lecture notes, but don't forget to add
division! Please also encode subtraction using addition and negation.

> aexp :: Parser AExp
> aexp = undefined


**HWHWHW** Now let's define a `BExp` parser; we'll have to use the
`aexp` parser. If you're unsure of which precedences to use here,
recall that in Boolean algebra, conjunction is multiplication and
disjunction is addition.

Your `bexp` parser should use the existing comparisons on `AExp`s to
encode all of the relevant comparisons. Here are some examples and
what they parse to; you don't have to parse *exactly* to what I do,
but you should parse to something equivalent. Use C-style syntax: and
is `&&`, or is `||`, and not is `!`. The constants can just use
lower-case names, like `true` and `false`.

> bexp_encoding_test1 = "x == y" -- Equal (Var x) (Var y)
> bexp_encoding_test2 = "x <= y" -- Or (Equal (Var x) (Var y)) (Lt (Var x) (Var y))
> bexp_encoding_test3 = "x != y" -- Not (Equal (Var x)) (Var y)
>
> bexp :: Parser BExp
> bexp = undefined


<h3>Problem 4: Parsing a la C</h3>

Now we can write a parser for statements. Let's use a C-like
syntax.

Your top-level parser should be called `cSyntax`. It's helpful to
think of a top-level program as being a sequence of one more
statements (in the concrete syntax, separated by `;`; in the abstract
syntax tree, joined by `Seq`).

Here are some example programs in the C-like syntax:

> cProg1 = "x = 5; y = 6;"
> cProg2 = "if (x == 0) { y = 10; } else { y = x; }"
> cProg3 = "while (iffy > 10) { iffy = iffy - 1; };\n\nderp = 7;"
> cProg4 = "while (true) {};"
> cProg5 = "if (x==0) { y = 10;} else { }\ny = x + y;\n;x = 5;"

You'll *definitely* want to write more test programs. If you come up
with interesting corner cases, post them to Piazza! I want to
explicitly encourage collaboration on understanding the syntax (though
please write your own parsers within your pair).

Let me reiterate that this is a *C-like* syntax, and not C. We just
want a syntax that "feels" like C.

> cSyntax :: Parser Stmt
> cSyntax = undefined


Note: you will *not* be penalized for extra `Skip`s in your parsed
output. If inserting a `Skip` here or there makes your life easier, go
for it.

<h3>Problem 5: Parsing a la Python</h3>

Python's syntax is pleasantly spare, a nice example of
*whitespace-sensitive syntax*. (You may notice whitespace is
meaningful in Haskell's syntax, as well!) The idea is simple:
developers already use indentation to indicate, e.g., how nested you
are in loops. Why not make such indentation a requirement and reduce
the clutter from curly brackets?

Robust languages can infer the indentation level used on a per-program
or per-block basis, but let's fix the indentation level at two: every
time we open an if or while block, we'll indent the blocks by two more
spaces than we were.

Here's a sample program:

> pyProg1 = "x = 5\ny = 6"

Oy---those newlines are going to get tiresome. There's a handy
function, `unlines`, which will let us write this a clearer way:

> pyProg1' = unlines ["x = 5",
>                     "y = 6"]
>
> pyProg2 = unlines ["if x == 0:",
>                    "  y = 10",
>                    "else:",
>                    "  y = x"]
> pyProg3 = unlines ["while iffy > 10: ",
>                    "  iffy = iffy - 1"]
> pyProg4 = unlines ["while false:",
>                    "  ",
>                    "x = 0"]
> pyProg5 = unlines ["while x > 0:",
>                    "  while y < x:",
>                    "    x = x - 1",
>                    "    y = y + 1",
>                    "  x = x - 1",
>                    ""]

I've tried to cover the corner cases with these examples, but please
do discuss them. I'll try to settle any ambiguities that may arise.

Please write another parser for our language, but in the style of
Python. Your top-level parser of statements should be named
`pythonSyntax`:

> pythonSyntax :: Parser Stmt
> pythonSyntax = undefined

You can largely follow the same idea as your C-style parser, but your
parsers for individual statements will need to keep track of the
current indentation level, e.g., by taking an integer
parameter. You'll need to be much more careful about whitespace in
general and newlines in particular. Writing a whitespace-sensitive
parser is hard. Let "testing" be your watchword!
