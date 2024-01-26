{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

-- |
-- Module      : Language.Pretty
-- Description : Pretty printer for L->
module Language.Pretty
    ( pprintInf
    , pprintPref
    , pprintNested
    ) where

import Prelude hiding ( (<>) )

import Language.Parser ( Prop(..), Nat(..) )

import Text.PrettyPrint.HughesPJ ( Doc, text, parens, render, nest, (<>), (<+>) )

{- ----------------------------------------------------------------------- -}
{- Pretty Printing -}

-- A `Prop` is an AST, so printing one in prefix or infix notation is a matter
-- of doing pre-order or in-order traversal over the input tree.

pprintInf :: Prop -> IO ()
pprintInf = putStrLn . render . pprInfix

pprintPref :: Prop -> IO ()
pprintPref = putStrLn . render . pprPrefix

pprInfix :: Prop -> Doc
pprInfix (Atom n)  = text $ showVar (toInt n)
pprInfix (p :-> q) = parens $ pprInfix p <+> text "->" <+> pprInfix q

pprPrefix :: Prop -> Doc
pprPrefix (Atom n)  = text $ showVar (toInt n)
pprPrefix (p :-> q) = text "C" <> pprPrefix p <> pprPrefix q

pprintNested :: String -> IO ()
pprintNested = putStrLn . render . nest 4 . text

-- | The `toInt` and `intAdd` functions were defined using the
-- 'worker/wrapper' transformation (cf. Gill and Hutton (2009)).
toInt :: Nat -> Int
toInt n = intAdd n 0

intAdd :: Nat -> Int -> Int
intAdd Zero m     = m
intAdd (Succ n) m = intAdd n (1 + m)

showVar :: Int -> String
showVar n = (\c -> [c]) $ ['p'..'z'] !! n
