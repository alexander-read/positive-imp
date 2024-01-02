module Main where

import System.IO ( stdout, hFlush )
import Control.Monad ( unless )

import Parser
import Pretty

-- TODO: use flags to determine infix or prefix parsing

main :: IO ()
main = do
    putStrLn "Welcome to the REPL!"
    putStrLn "To quit, type ':q'"
    mainAux

mainAux :: IO ()
mainAux = do
    expr <- greet
    unless (expr == ":q") $ pprintInf (eval expr) >> mainAux

greet :: IO String
greet = do
    putStr "Implication> "
    hFlush stdout
    getLine

eval :: String -> Prop
eval str = fst $ (parse parsePrefix str) !! 0
