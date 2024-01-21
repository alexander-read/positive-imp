{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Language.Parser
-- Description : Parser for the language L->
module Language.Parser
    ( Prop (..)
    , Nat (..)
    , runParser
    , parseProp
    ) where

import Control.Applicative ( Alternative, empty, (<|>) )
import Data.Char ( ord )

-- TODO: add QuickCheck tests for parsing

{- ----------------------------------------------------------------------- -}
{- Grammar -}

data Prop = Atom Nat
           | Prop :-> Prop
           deriving (Eq, Ord)
infixr 4 :-> -- implication is right associative

data Nat = Zero | Succ Nat deriving (Eq, Ord)

{- ----------------------------------------------------------------------- -}
{- Parsing -}

{-------- Types and Instances --------}

newtype Parser a = Parser { runParser :: (String -> [(a, String)]) }

instance Functor Parser where
  fmap f p = Parser $ \cs -> [(f c,cs') | (c,cs') <- runParser p cs]

instance Applicative Parser where
  pure c  = Parser $ \cs -> [(c,cs)]
  f <*> p = Parser $ \cs -> [(f' xs, rest') | (f', rest)  <- runParser f cs
                                            , (xs, rest') <- runParser p rest]

instance Monad Parser where
  return  = pure
  p >>= f = Parser $ \cs -> concat [runParser (f a) cs' | (a,cs') <- runParser p cs]

instance Alternative Parser where
  empty   = Parser $ \_ -> []
  p <|> q = Parser $ \cs -> runParser p cs ++ runParser q cs

{-------- Parser Combinators (cf. Hutton and Meijer) --------}

-- | Parse first character of a String
item :: Parser Char
item = Parser $ \case
    ""     -> []
    (c:cs) -> [(c,cs)]

-- | Build a parser of characters that satisfy a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= cond
  where
    cond :: Char -> Parser Char
    cond c = if p c then return c else empty

-- | Build a parser of characters identical to the input
char :: Char -> Parser Char
char = satisfy . (==)

-- | Take a parser of as and return a parser of sequences of as
many :: Parser a -> Parser [a]
many p = do {x <- p; xs <- many p; return (x:xs)} <|> return []

string :: String -> Parser String
string ""     = return ""
string (x:xs) = char x >> string xs >> return (x:xs)

-- | Remove whitespace
space :: Parser String
space = many $ satisfy isSpace
  where
    isSpace :: Char -> Bool
    isSpace s = (s == ' ' || s == '\n' || s == '\t')

token :: Parser a -> Parser a
token p = p <* space >>= return

symbol :: String -> Parser String
symbol = token . string

{-------- Parsers for the Grammar --------}

-- | Parse the conditional operator
parseImp :: Parser (Prop -> Prop -> Prop)
parseImp = (symbol "C" <|> symbol "->") >> return (:->)

-- | Propositional atoms
atom :: Parser Char
atom = satisfy (`elem` ['p'..'z'])

-- | Parse an atom. Using `Int` might be simpler, but Peano
-- numbers match the gramar for L-> in a nice way
parseAtom :: Parser Prop
parseAtom = do {var <- token atom; return $ Atom (numeral var)}
  where
    numeral                  = church Succ Zero . index
    index                    = (`mod` 112) . ord
    church f x n | n == 0    = x
                 | otherwise = f $ church f x (n - 1)

-- | Parse a prefix-style formula (adapted from `chainr` in Parsec)
parsePrefix :: Parser Prop
parsePrefix = parseCompound <|> parseAtom
  where
    parseCompound :: Parser Prop
    parseCompound = do
      c <- parseImp
      x <- parsePrefix
      y <- parsePrefix
      return (c x y)

-- | Parse an infix-style formula
parseInfix :: Parser Prop
parseInfix = parseCompound <|> parseAtom
  where
    parseCompound :: Parser Prop
    parseCompound = do
      x <- symbol "(" *> parseInfix
      c <- parseImp
      y <- parseInfix <* symbol ")"
      return (c x y)

-- | Parse an infix- or prefix-style formula
parseProp :: Parser Prop
parseProp = parsePrefix <|> parseInfix
