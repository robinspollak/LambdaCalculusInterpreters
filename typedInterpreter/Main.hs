module Main where

import Control.Applicative
import Control.Monad()
import Control.Monad.IO.Class()

import Data.Array.IO()
import Data.List.Split
import Data.List hiding (filter)
import qualified Data.Map as Map

import System.Environment
import System.IO()
import System.Console.GetOpt

import Data
import ParseBoilerplate
import Parse
import Type
import Eval

lcparser :: String -> Either Error LC
lcparser s = case parse Parse.parseL s of
             Just (x, "")  -> Right x
             Just (_, str) -> Left $ ParseError str
             Nothing       -> Left $ ParseError s
                 
interp :: LC -> Either Error LC
interp = evalCBV Map.empty

data Flag = Help | Unsafe deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
            Option "?" ["help"] (NoArg Help) "help message",
            Option "u" ["unsafe"] (NoArg Unsafe) "disable type checking"
          ]

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv = case getOpt Permute options argv of
                      (o,n,[]) -> return (o,n)
                      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
                    where header = "Usage: ./interp [OPTION...] file"

helpMessage = "interp [OPTIONS] FILE (defaults to -, for stdin) \n lambda calculus interpreter \n\n -u --unsafe    Run without type checking  \n -? --help     Display help message"

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
                        parsed    <- lcparser $ intercalate "\n" (filter (\x -> x/="") (splitOn "\n" inp))
                        checked   <- if Unsafe `elem` flags then return parsed else typeCheck parsed
                        interped  <- interp $ removeAscr checked
                        return $ show interped
