module Main where

import Parser

main :: IO ()
main = do
    expr <- getLine
    print $ fst $ (parse parsePrefix expr) !! 0
