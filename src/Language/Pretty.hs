{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

----------------------------------------------------------------------------
-- |
-- Module      : Language.Pretty
-- Description : Pretty printer for L->
--
----------------------------------------------------------------------------
module Language.Pretty
    ( pprInf
    , pprPref
    , pprSubst
    , pprVals
    , pprError
    , pprNested
    ) where

import Prelude hiding ( (<>) )

import Language.Grammar
import Language.State

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Text.PrettyPrint.HughesPJ

{- ----------------------------------------------------------------------- -}
{- Propositional Formulae and Values -}

-- | Pretty print a proposition in infix notation
pprInf :: Prop -> IO ()
pprInf = putStrLn . render . pprintInfix

-- | Pretty print a proposition in prefix notation
pprPref :: Prop -> IO ()
pprPref = putStrLn . render . pprintPrefix

-- | In-order traversal of the AST
pprintInfix :: Prop -> Doc
pprintInfix (Atom n)  = text $ name n
pprintInfix (p :-> q) = parens $ pprintInfix p <+> text "->" <+> pprintInfix q

-- | Pre-order traversal of the AST
pprintPrefix :: Prop -> Doc
pprintPrefix (Atom n)  = text $ name n
pprintPrefix (p :-> q) = text "C" <> pprintPrefix p <> pprintPrefix q

-- | Pretty print the variables
pprVals :: Values -> IO ()
pprVals vs = putStrLn $ render $ brackets $ vcat $ pprintVals $ IntMap.toList $ vals vs

pprintVals :: [(Int, Prop)] -> [Doc]
pprintVals xs = punctuate (text ",") (pprVal <$> xs)
  where
    pprVal (n, p) = (text $ show n) <+> char '=' <+> pprintPrefix p

{--------------------------------------------------------------------------}
{- Substitutions -}

-- | Pretty print a substitution
pprSubst :: Subst -> IO ()
pprSubst = putStrLn . render . brackets . vcat . pprPair . Map.toList

pprPair :: [(PVar, Prop)] -> [Doc]
pprPair xs = punctuate (text ",") (pprSub <$> xs)
  where
    pprSub (t, v) = (text $ name t) <+> text "|->" <+> (pprintInfix $ v)

{--------------------------------------------------------------------------}
{- Utility Functions -}

-- | Pretty print errors
pprError :: PropError -> IO ()
pprError = putStrLn . render . pprintErr
  where
    pprintErr err = case err of
        (DErr p q) -> pprintInfix q <+> text "is not detachable from" <+> pprintInfix p
        (UErr p q) -> pprintInfix p <+> text "cannot be unified with" <+> pprintInfix q

pprNested :: String -> IO ()
pprNested = putStrLn . render . nest 4 . text
