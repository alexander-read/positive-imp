{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad ( unless )
import System.IO ( stdout, hFlush )

import Language.Parser
import Language.Pretty

-- | Simple error handling
newtype Error = InvalidExpr { msg :: String }

main :: IO ()
main = do
    putStrLn "Welcome to the REPL!"
    putStrLn "To quit, type ':q'"
    mainAux

mainAux :: IO ()
mainAux = do
    expr <- greet
    unless (expr == ":q") $ case eval expr of
        Right tree -> putStr "=== " >> pprintInf tree >> mainAux
        Left error -> putStrLn (msg error) >> mainAux

greet :: IO String
greet = do
    putStr "L-> "
    hFlush stdout
    getLine

-- The parser fails if it returns the empty list
eval :: String -> Either Error Prop
eval str = case runParser parseProp str of
    []    -> Left  $ InvalidExpr "Invalid expression!"
    (p:_) -> Right $ fst p
