module Main where

import Control.Monad()
import Control.Monad.IO.Class()
import Control.Applicative

import Data.Array.IO()
import Data.List.Split
import Data.List
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map()
import Data.Map()

import System.Environment
import System.IO()
import System.Console.GetOpt

import Parse
import Eval
import Data

convertChurch :: LC -> Either Error Int
convertChurch x = case churched of
                   Right (Num n) -> Right n
                   Right Succ    -> Right 1
                   Right x       -> Left $ BadChurch x
                   Left e        -> Left e
                  where churched = evalCBV $ App (App x Succ) (Num 0)

convertInt :: Int -> LC
convertInt n = Lambda "s" (Lambda "z" (convertHelp n))
           where convertHelp 0 = Var "z"
                 convertHelp n = App (Var "s") $ convertHelp (n - 1)

convertInts :: LC -> Either Error LC
convertInts Succ    = Right Succ
convertInts (Num n) = Right $ convertInt n
convertInts (Var x) = Right $ Var x
convertInts (Lambda v lc) = do
                            lc' <- convertInts lc
                            Right $ Lambda v lc'
convertInts (App lc1 lc2) = do
                            lc1' <- convertInts lc1
                            lc2' <- convertInts lc2
                            Right $ App lc1' lc2'
convertInts (Let v lc1 lc2) = do
                              lc1' <- convertInts lc1
                              lc2' <- convertInts lc2
                              Right $ Let v lc1' lc2'

bv :: LC -> (Set VarName)
bv (Var _) = Set.empty
bv Succ    = Set.empty
bv (Num _) = Set.empty
bv (Lambda x lc)   = Set.union (Set.singleton x) (bv lc)
bv (Let _ lc1 lc2) = Set.union (bv lc1) (bv lc2)
bv (App lc1 lc2)   = Set.union (bv lc1) (bv lc2)

allVars :: LC -> (Set VarName)
allVars (Var x) = Set.singleton x
allVars Succ    = Set.empty
allVars (Num _) = Set.empty
allVars (Lambda x lc)   = Set.union (Set.singleton x) (allVars lc)
allVars (Let l lc1 lc2) = Set.difference (Set.union (allVars lc1) (allVars lc2)) (Set.singleton l)
allVars (App lc1 lc2)   = Set.union (allVars lc1) (allVars lc2)

ubv :: LC -> (Set VarName)
ubv lc = Set.difference (allVars lc) (bv lc)

data Flag = Help | Numeral | Check deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
            Option "?" ["help"] (NoArg Help) "help message",
            Option "n" ["numeral"] (NoArg Numeral) "church numeral conversion",
            Option "c" ["check"] (NoArg Check) "check for unbound variables"
          ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = case getOpt Permute options argv of
                      (o,n,[]) -> return (o,n)
                      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
                    where header = "Usage: ic [OPTION...] file"

helpMessage = "interp [OPTIONS] FILE (defaults to -, for stdin) \n lambda calculus interpreter \n\n -c --check    Check scope \n -n --numeral  Convert final Church numeral to a number \n -? --help     Display help message"

main :: IO ()
main = do
       args <- getArgs
       opts <- compilerOpts args
       if (Help `elem` fst opts) then putStrLn helpMessage else case opts of
                    (flags, [])    -> handleInput flags <$> getContents >>= convertToIO >>= putStrLn
                    (flags, ["-"]) -> handleInput flags <$> getContents >>= convertToIO >>= putStrLn
                    (flags, [fp])  -> handleInput flags <$> (readFile fp) >>= convertToIO >>= putStrLn
                    _              -> fail "Please provide a file or data on stdin for us to interpret"

convertToIO :: Either Error String -> IO String
convertToIO a = do
                string <- case a of
                          (Left e) -> fail $ show e
                          (Right s) -> return s
                return string

handleInput :: [Flag] -> String -> Either Error String
handleInput flags inp = do
                        parsed    <- intermediateHandling flags inp
                        interped  <- interp parsed
                        if Numeral `elem` flags
                        then show <$> (convertChurch interped)
                        else return $ show interped

intermediateHandling :: [Flag] -> String -> Either Error LC
intermediateHandling flags inp = do
                                 let input = intercalate "\n" (filter (\x -> x/="") (splitOn "\n" inp))
                                 parsed  <- lcparser input
                                 numeral <- if Numeral `elem` flags then convertInts parsed else return parsed
                                 check   <- if Check `elem` flags && (ubv numeral /= Set.empty)
                                            then Left $ (UnboundVariables (Set.toList (ubv numeral)))
                                            else return numeral
                                 return check
