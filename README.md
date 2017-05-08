# Lambda Calculus Interpreters
This project consists of two interpreters for the lambda calculus. Each is contained within its own directory and can
be compiled into an executable by running `make` in the proper directory. Then content can either be read from a file or 
from STDIN by running the command `./interp`
## Untyped Lambda Calculus Interpreter
This is a simple implementation of the lambda calculus, suitable for use with church numerals, church booleans, and church pairs.
Some examples of acceptable programs:
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
## Typed Lambda Caclulus Interpreter
# Contributions
