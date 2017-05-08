# Lambda Calculus Interpreters
This project consists of two interpreters for the lambda calculus, both implemented with a call-by-value evaluation
scheme. Each is contained within its own directory and can be compiled into an executable by running `make` in the proper
directory. Then content can either be read from a file or from STDIN by running the command `./interp` Additionally, each
directory contains a file `fact.lc` that, when evaluated, computes the factorial of 5. This file may be especially useful
in building up some primitives for the untyped lambda calculus.
## Untyped Lambda Calculus Interpreter
This is a simple implementation of the lambda calculus, suitable for use with church numerals, church booleans, and church pairs, and the Y combinator. The language is as follows:
```
e ::= x | e1 e2 | lambda x. e
```
Some examples of acceptable programs are:
```
(lambda x y. x) (lambda z. z)
```
```
let id = lambda x. x in
lambda y. id
```
```
let zero = lambda s z. z in
let succ = lambda n. lambda s z. s (n s z) in
succ (succ zero)
```
Some examples of an uncceptable programs are:
```
lambda. lambda lambda
```
This is nonsensical.
```
(lambda x. y) (lambda x. x)
```
This will yield an unbound variable exception.
## Typed Lambda Caclulus Interpreter
This is an interpreter for a much more expressive language that includes and typechecks ints, bools, pairs, and recursive
functions without the Y combinator. The language is as follows:
```
e ::= x | e1 e2 | lambda x : t. e
     | if e1 then e2 else e3 | let x = e1 in e2 | let rec x : t = e1 in e2 | (e : t)
     | n | true | false | (e1, e2)
     | unop e | e1 binop e2
unop ::= not | neg | fst | snd
binop ::= + | - | * | / | and | or | == | /= | < | <= | > | >=
```
Some examples of acceptable programs are:
```
(lambda x:int. x) 10 * 20
```
```
(1,2) == (0+1,3-1)
```
```
let rec fact:(int -> int) = lambda n:int. if n==1 then 1 else n*(fact (n-1) )in fact 5
```
Some examples of unacceptable programs are:
```
true < false
```
Comparators are not defined on booleans.
```
1 + true
```
This will yield a type mismatch error.
```
let rec x : int = x + 1 in x
```
This will yield a recursion error, as `x` is not a function.
# Contributions
Suggestions, comments, issues, and pull requests are all encouraged! I can be reached at [robin@pollak.io](mailto:robin@pollak.io) and my website can be found [here](http://robin.pollak.io)
