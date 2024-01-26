{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

-- |
-- Module      : Language.Repl
-- Description : A REPL for L->
module Language.Repl ( main ) where

import Language.Parser
import Language.Pretty

import System.IO ( stdout, hFlush )

import Data.Function ( (&) )
import Data.Map.Strict as Map

-- TODO: use `System.Console.Haskeline` to be a bit more sophisticated
-- Add some more pretty printing for logical formulae

main :: IO ()
main = do
    putStrLn "Welcome to the REPL for L->."
    putStrLn "Type `:h` for a list of commands."
    mainAux

-- | Command line prompt
getInputLine :: String -> IO String
getInputLine prompt = do
    putStr prompt
    hFlush stdout >> getLine

-- | The loop for the REPL
mainAux :: IO ()
mainAux = do
    input <- getInputLine "φ> "
    case input of
        (':' : rest) -> runCommand rest
        expr         -> runProp expr

-- | Simple error handling
data Error = InvalidExpr { msg :: String }
           | InvalidCmd  { msg :: String }

{--------------------------------------------------------------------------}
{- Parsing the Language -}

-- | Run a proposition. At the moment this just parses an expression and
-- pretty prints the AST using infix notation. Eventually, I'd like it
-- to do condensed detachment instead, with the parsing-only options
-- left to the utility commands.
runProp :: String -> IO ()
runProp str = case parse str of
    Right tree -> putStr "=== " >> pprintInf tree >> mainAux
    Left err   -> putStrLn (msg err) >> mainAux

-- | Parse an expression. If the parser fails it returns the empty list
parse :: String -> Either Error Prop
parse str = case runParser parseProp str of
    []    -> Left  $ InvalidExpr "Invalid expression!"
    (p:_) -> Right $ fst p

{--------------------------------------------------------------------------}
{- Utility Commands -}

-- | Run a command from the options
runCommand :: String -> IO ()
runCommand = getCommand commands
  where
    getCommand options str = case options Map.!? str of
        Nothing  -> display (InvalidCmd "Invalid command!") >> mainAux
        Just cmd -> cmd str
    display = putStrLn . msg -- Add colour?

-- | The REPL command options
commands :: Map String (String -> IO ())
commands = [ ("q",      quitRepl)
           , ("h",      helpRepl)
           , ("prefix", prefixRepl)
           ] & Map.fromList

quitRepl :: String -> IO ()
quitRepl _ = putStrLn "Leaving L->" >> return ()

prefixRepl :: String -> IO ()
prefixRepl _ = do
    expr <- getInputLine "φ> "
    case parse expr of
        Right tree -> putStr "=== " >> pprintPref tree >> mainAux
        Left err   -> putStrLn (msg err) >> mainAux

helpRepl :: String -> IO ()
helpRepl _ = do
    putStrLn     " Commands in the L-> REPL:"
    pprintNested ":q       Quits the REPL"
    pprintNested ":prefix  Parses an expression, prints AST in prefix format"
    pprintNested ":infix   Parses an expression, prints AST in infix format"
    pprintNested ":h       Displays the command options"
    mainAux
