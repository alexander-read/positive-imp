module Main where

import System.IO ( stdout, hFlush )
import Control.Monad ( unless )

import Parser
import Pretty

-- TODO: use flags to determine infix or prefix parsing

newtype Error = InvalidExpr { msg :: String } -- add simple error for now

main :: IO ()
main = do
    putStrLn "Welcome to the REPL!"
    putStrLn "To quit, type ':q'"
    mainAux

mainAux :: IO ()
mainAux = do
    expr <- greet
    unless (expr == ":q") $ case eval expr of
        Right tree -> pprintInf tree >> mainAux
        Left error -> (putStrLn $ msg error) >> mainAux

greet :: IO String
greet = do
    putStr "Implication> "
    hFlush stdout
    getLine

-- The parser fails if it returns the empty list
eval :: String -> Either Error Prop
eval str = case parse parsePrefix str of
    []    -> Left  $ InvalidExpr "Invalid expression!"
    (p:_) -> Right $ fst p
