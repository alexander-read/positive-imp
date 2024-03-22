{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

----------------------------------------------------------------------------
-- |
-- Module      : Language.Repl
-- Description : A REPL for L->
--
-- TODO: could use 'tagless final' style for the monad transformer to
-- to reduce the noise of using `lift` everywhere
--
----------------------------------------------------------------------------
module Language.Repl ( main ) where

import Language.Grammar
import Language.Parser
import Language.Pretty
import Language.State

import System.IO ( stdout, hFlush )

import Data.Char ( isSpace )
import Data.Function ( (&) )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

import Control.Monad.Trans ( lift )
import Control.Monad.Trans.State.Strict

main :: IO ()
main = do
    putStrLn "Welcome to the REPL for L->."
    putStrLn "Type `:h` for a list of commands."
    evalStateT mainAux emptyVals

-- | Command line prompt
getInputLine :: String -> IO String
getInputLine prompt = do
    putStr prompt
    hFlush stdout >> getLine

-- | The loop for the REPL
mainAux :: StateT Values IO ()
mainAux = do
    input <- lift $ getInputLine "φ> "
    case input of
        (':' : rest) -> runCommand rest
        expr         -> runValue expr >> mainAux

-- | Simple error handling
data Error = InvalidExpr { msg :: String } | InvalidCmd  { msg :: String }

-- | The 'default' state of the REPL is to try to parse a value declaration.
-- If successful the value is added to the global state
runValue :: String -> StateT Values IO ()
runValue str = case parseVal str of
    Right val -> modify $ addVal val
    Left err  -> lift $ putStrLn (msg err)

-- | Parse a value declaration
parseVal :: String -> Either Error Value
parseVal str = case runParser parseValue str of
    []    -> Left  $ InvalidExpr "Invalid value declaration!"
    (v:_) -> Right $ fst v

-- | Parse a formula. If the parser fails it returns the empty list
parse :: String -> Either Error Prop
parse str = case runParser parseProp str of
    []    -> Left  $ InvalidExpr "Invalid formula!"
    (p:_) -> Right $ fst p

{--------------------------------------------------------------------------}
{- Utility Commands -}

-- | Run a command from the options.
-- This is roughly how GHC's REPL does it
runCommand :: String -> StateT Values IO ()
runCommand = getCommand commands
  where
    getCommand options input =
        let (command, exprs) = break isSpace input in
            case options Map.!? command of
                Nothing  -> (lift $ display (InvalidCmd "Invalid command!")) >> mainAux
                Just cmd -> cmd $ trimFront exprs
    display = putStrLn . msg

-- | The REPL command options
commands :: Map.Map String (String -> StateT Values IO ())
commands = [ ("d",    condCmd)
           , ("h",    helpCmd)
           , ("i",    infxCmd)
           , ("p",    prefCmd)
           , ("q",    quitCmd)
           , ("v",    valsCmd)
           ] & Map.fromList

-- | Exit the REPL
quitCmd :: String -> StateT Values IO ()
quitCmd _ = lift $ putStrLn "Leaving L->" >> return ()

-- | Display the current values in state
valsCmd :: String -> StateT Values IO ()
valsCmd _ = get >>= lift . pprVals >> mainAux

-- | Pretty print an expression in infix notation
infxCmd :: String -> StateT Values IO ()
infxCmd = pprintExpr pprInf

-- | Pretty print an expression in prefix notation
prefCmd :: String -> StateT Values IO ()
prefCmd = pprintExpr pprPref

-- | Printer for the REPL parameterised over a pretty printer
pprintExpr :: (Prop -> IO ()) -> String -> StateT Values IO ()
pprintExpr printer expr = do
    case parse expr of
        Right tree -> lift $ putStr "φ> " >> printer tree
        Left err   -> lift $ putStrLn (msg err)
    mainAux

-- | Computes the condensed detachment of two formulae, and
-- stores the result as a fresh value in the state
condCmd :: String -> StateT Values IO ()
condCmd exprs = let (p, q) = trimFront <$> break isSpace exprs in do
        vals'@(Values state') <- get
        case detachAux (read p) (read q) vals' of
            Left err       -> lift $ putStrLn $ message err
            Right (p', q') -> let c = condense p' q' in do
                _ <- modify $ addVal (Val (getFresh state') c)
                lift $ putStr "φ> " >> (pprPref c)
        mainAux

-- | Get a 'fresh' value name for the state
getFresh :: IntMap.IntMap Prop -> Int
getFresh m = case IntMap.lookupMax m of
    Nothing     -> 1
    Just (k, _) -> k + 1

detachAux :: Int -> Int -> Values -> Either LookupErr (Prop, Prop)
detachAux p q values = case (lookupVal p values, lookupVal q values) of
    (Left err, _)        -> Left err
    (_, Left err)        -> Left err
    (Right p1, Right p2) -> Right (p1, p2)

helpCmd :: String -> StateT Values IO ()
helpCmd _ = do
    lift $ putStrLn  " Commands in the L-> REPL:"
    lift $ pprNested ":q                Quits the REPL"
    lift $ pprNested ":v                Displays the current value declarations"
    lift $ pprNested ":d <val> <val>    Computes the condensed detachment of two formulae"
    lift $ pprNested ":i <expr>         Parses an expression, prints AST in infix format"
    lift $ pprNested ":p <expr>         Parses an expression, prints AST in prefix format"
    lift $ pprNested ":h                Displays the command options"
    lift $ putStrLn  "You can create a globally scoped value with: `<val> = <expr>`"
    lift $ putStrLn  "where a `val` is any numeric string"
    mainAux

-- | From Data.String.Utils, need to fix import
trimFront :: String -> String
trimFront str = case str of
    []     -> []
    (x:xs) -> if elem x " \t\n\r" then trimFront xs else str
