{-# LANGUAGE LambdaCase #-}

module Parser where

import Data.Maybe (fromJust)
import Data.List (elemIndex)

{- ----------------------------------------------------------------------- -}
{- Grammar -}

data Prop = Atom Nat
           | Prop :-> Prop
           deriving (Eq, Ord)
infixr 4 :-> -- implication is right associative

data Nat = Zero | Succ Nat deriving (Eq, Ord)

{- ----------------------------------------------------------------------- -}
{- Parsing -}

{- We follow the implementation of monadic parser combinators in Hutton and
   Meijer (1996, 1998). The Functor and Applicative operations are not used,
   but we need the instance declarations for the monad class declaration.
-}

{-------- Types and Instances --------}

newtype Parser a = Parser (String -> [(a, String)])

type Predicate a = a -> Bool

-- | Unwrap the newtype wrapper (HM, 1998: 3)
parse :: Parser a -> (String -> [(a, String)])
parse (Parser p) = p

instance Functor Parser where
    fmap f p = Parser $ \cs -> [(f c,cs') | (c,cs') <- parse p cs]

instance Applicative Parser where
    pure c  = Parser $ \cs -> [(c,cs)]
    f <*> p = Parser $ \cs -> [(f' output, rest') | (f',rest) <- parse f cs, (output,rest') <- parse p rest]

instance Monad Parser where
    return  = pure
    p >>= f = Parser $ \cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs]

class Monad m => MonadZero m where
    zero :: m a

class MonadZero m => MonadPlus m where
    (++) :: m a -> m a -> m a

instance MonadZero Parser where
    zero = Parser $ \cs -> []

instance MonadPlus Parser where
    -- | Non-deterministic choice operator
    p ++ q = Parser $ \cs -> parse p cs Prelude.++ parse q cs

{-------- Parser Combinators (cf. Hutton and Meijer) --------}

-- | Parse first character of a String
item :: Parser Char
item = Parser $ \case
                  ""     -> []
                  (c:cs) -> [(c,cs)]

-- | Build a parser of characters that satisfy a predicate
satisfy :: Predicate Char -> Parser Char
satisfy p = do {c <- item; if p c then return c else zero}

-- | Build a parser of characters identical to x
char :: Char -> Parser Char
char x = satisfy (\y -> x == y)

-- | Take a parser of as and return a parser of sequences of as
many :: Parser a -> Parser [a]
many p = do {x <- p; xs <- many p; return (x:xs)} Parser.++ return []

string :: String -> Parser String
string ""     = do {return ""}
string (x:xs) = do {char x; string xs; return (x:xs)}

-- | Remove whitespace
space :: Parser String
space = many $ satisfy isSpace
  where
    isSpace s = (s == ' ' || s == '\n' || s == '\t')

token :: Parser a -> Parser a
token p = do {value <- p; space; return value}

symbol :: String -> Parser String
symbol xs = token $ string xs

{-------- Parsers for the Grammar --------}

parseImp :: Parser (Prop -> Prop -> Prop)
parseImp = do {symbol "C"; return (:->)}

atom :: Parser Char
atom = satisfy (\y -> 'p' <= y && y <= 'z')

parseAtom :: Parser Prop
parseAtom = do {var <- token atom; return $ Atom (numeral var)}
  where
    numeral v = (iterate Succ Zero) !! index v -- make this not partial?
    index v = fromJust $ elemIndex v ['p'..'z']

-- | Adapted from the implementation of `chainr` in parsec
parsePrefix :: Parser Prop
parsePrefix = scan
          where
            scan   = do {c <- parseImp; rest c} Parser.++ do {z <- parseAtom; return z}
            rest c = do {x <- scan
                        ;y <- scan
                        ;return (c x y)
                        }

-- We might need to parse multiple occurrences of the implication operator
-- before parsing any atoms. That is, either operand to the initial 'C' might
-- be nested, so we recurse on both, eventually finding an atom, in 'rest imp'.

{- ----------------------------------------------------------------------- -}
{- Pretty printing -}

instance Show Nat where
    show Zero     = showVar (toInt Zero)
    show (Succ n) = showVar (toInt (Succ n))

instance Show Prop where
    show (Atom n)  = show n
    show (p :-> q) = showSymbol " -> " p q

-- | The `toInt` and `intAdd` functions were defined using the
-- 'worker/wrapper' transformation (cf. Gill and Hutton (2009)).
toInt :: Nat -> Int
toInt n = intAdd n 0

intAdd :: Nat -> Int -> Int
intAdd Zero m     = m
intAdd (Succ n) m = intAdd n (1 + m)

showVar :: Int -> String
showVar n = (\c -> [c]) $ ['p'..'z'] !! n

showSymbol :: String -> Prop -> Prop -> String
showSymbol op p q = "(" Prelude.++ show p Prelude.++ op Prelude.++ show q Prelude.++ ")"
