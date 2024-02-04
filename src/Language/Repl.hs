{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

----------------------------------------------------------------------------
-- |
-- Module      : Language.Repl
-- Description : A REPL for L->
--
-- TODO: use `System.Console.Haskeline` to be a bit more sophisticated
-- Pretty print condensed detachments using Meredith's 'D'-notation?
--
----------------------------------------------------------------------------
module Language.Repl ( main ) where

import Language.Grammar
import Language.Parser
import Language.Pretty

import System.IO ( stdout, hFlush )

import Data.Function ( (&) )
import Data.Map.Strict as Map

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
data Error = InvalidExpr { msg :: String } | InvalidCmd  { msg :: String }

{--------------------------------------------------------------------------}
{- Parsing the Language -}

-- | Run a proposition. At the moment this just parses an expression and
-- pretty prints the AST using infix notation. Eventually, I'd like to
-- store formulae in variables instead, which can then be used for the
-- parsing and detachment options in the utility commands
runProp :: String -> IO ()
runProp str = case parse str of
    Right tree -> putStr "=== " >> pprInf tree >> mainAux
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
    display = putStrLn . msg

-- | The REPL command options
commands :: Map String (String -> IO ())
commands = [ ("q",      quitRepl)
           , ("h",      helpRepl)
           , ("prefix", prefixRepl)
           , ("infix",  infixRepl)
           , ("d",      detachRepl)
           , ("cd",     condenseRepl)
           ] & Map.fromList

quitRepl :: String -> IO ()
quitRepl _ = putStrLn "Leaving L->" >> return ()

prefixRepl :: String -> IO ()
prefixRepl _ = do
    expr <- getInputLine "φ> "
    case parse expr of
        Right tree -> putStr "=== " >> pprPref tree >> mainAux
        Left err   -> putStrLn (msg err) >> mainAux

infixRepl :: String -> IO ()
infixRepl _ = do
    expr <- getInputLine "φ> "
    case parse expr of
        Right tree -> putStr "=== " >> pprInf tree >> mainAux
        Left err   -> putStrLn (msg err) >> mainAux

detachRepl :: String -> IO ()
detachRepl _ = do
    p <- getInputLine "φ> P1. "
    q <- getInputLine "φ> P2. "
    case detachAux p q of
        Left err       -> putStrLn (msg err) >> mainAux
        Right (p1, p2) -> case detach p1 p2 of
            Right prop -> putStr "φ> C.  " >> pprInf prop >> mainAux
            Left err   -> pprError err >> mainAux

condenseRepl :: String -> IO ()
condenseRepl _ = do
    p <- getInputLine "φ> P1. "
    q <- getInputLine "φ> P2. "
    case detachAux p q of
        Left err       -> putStrLn (msg err) >> mainAux
        Right (p1, p2) -> putStr "φ> C.  " >> (pprInf $ condense p1 p2) >> mainAux
        -- Could also pretty print the mgu?

detachAux :: String -> String -> Either Error (Prop, Prop)
detachAux p q = case (parse p, parse q) of
    (Left err, _)        -> Left err
    (_, Left err)        -> Left err
    (Right p1, Right p2) -> Right (p1, p2)

helpRepl :: String -> IO ()
helpRepl _ = do
    putStrLn  " Commands in the L-> REPL:"
    pprNested ":q       Quits the REPL"
    pprNested ":d       Computes the detachment of two formulae"
    pprNested ":cd      Computes the condensed detachment of two formulae"
    pprNested ":prefix  Parses an expression, prints AST in prefix format"
    pprNested ":infix   Parses an expression, prints AST in infix format"
    pprNested ":h       Displays the command options"
    mainAux
