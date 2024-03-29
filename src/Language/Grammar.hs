{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE LambdaCase #-}

----------------------------------------------------------------------------
-- |
-- Module      : Language.Grammar
-- Description : Grammar for the language L->
--
-- This module implements the 'core' of Hindley-Milner type-inference, the
-- Unification algorithm, for L->. Then, Unification is used to implement
-- C.A. Meredith's rule of condensed detachment. For relevant material, see:
--
-- * Hindley, J.R. & Meredith, D. (1990), 'Principal Type-Schemes and
--   Condensed Detachment', The Journal of Symbolic Logic, 55(1):90–105
--
----------------------------------------------------------------------------
module Language.Grammar
  ( Prop (..)
  , PVar (..)
  , PropError (..)
  , Subst
  , detach
  , condense
  , unify
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

{--------------------------------------------------------------------------}
{- Grammar -}

-- | Given Curry-Howard, Prop ~ Type, so `Prop :-> Prop` is like the usual
-- `Type :-> Type`, and `Atom`s are essentially type variables
data Prop = Atom PVar
          | Prop :-> Prop
          deriving (Eq, Ord, Show)
infixr 4 :-> -- implication is right associative

-- | Propositional variables
newtype PVar = P { name :: String } deriving (Eq, Ord, Show)

-- | A mapping from distinct variables {a1,...,an} to propositional
-- formulae {p1,...,pn}. Usually denoted: [pi/ai], or [ai |-> pi].
type Subst = Map.Map PVar Prop

-- | A type for 'fresh' variable names (Pierce, 2002: 326)
data NextUVar = NextUVar { fresh :: String, next :: UVarGenerator }

type UVarGenerator = () -> NextUVar

-- | Simple error handling for detachments and unification
data PropError = DErr Prop Prop | UErr Prop Prop

{--------------------------------------------------------------------------}
{- Substitutions -}

-- | Get the free variables in a propositional formula. Note that there are
-- no binders in the object language, so this is simply all of the variables
fv :: Prop -> Set.Set PVar
fv (Atom n)  = Set.singleton n
fv (p :-> q) = Set.union (fv p) (fv q)

-- | Apply a substitution to a propositional formula. This simultaneously
-- substitutes {p1,...,pn} for distinct {a1,...,an} (HM, 1990: Rule (Sub))
apply :: Subst -> Prop -> Prop
apply sub p = case p of
  Atom a -> case sub Map.!? a of
    Nothing -> Atom a
    Just q  -> q
  p1 :-> p2 -> (apply sub p1) :-> (apply sub p2)

-- | Compose two substitutions. Apply s to all values in g, then add any
-- mappings in s not in g, so `s ... g` === `s(g(p))`
(...) :: Subst -> Subst -> Subst
s ... g = thread s g `Map.union` s
  where
    thread s1 s2 = Map.map (apply s1) s2

-- | Given propositional formulae q and p, generate a re-naming for q so that it
-- has no variables in common with p. This is useful for condensed detachment
rename :: Prop -> Prop -> Subst
rename q p = genFreshSubst commonVars q varGen
  where
    commonVars = Set.toList $ fv p `Set.intersection` fv q

-- | Given a list of variables and a formula, generate a substitution from variables
-- in the list to fresh variables that do not occur in the formula
genFreshSubst :: [PVar] -> Prop -> UVarGenerator -> Subst
genFreshSubst xs q gen = go xs xs gen
  where
    go []     _  _ = Map.empty
    go _      [] _ = Map.empty
    go (y:ys) zs g = go ys (newVar : zs) (getNextGen g) ... Map.fromList [(y, Atom newVar)]
      where
        newVar = snd $ newVariable (zs ++ (Set.toList $ fv q)) gen

-- Check that the fresh generated variable is not in [PVar]
newVariable :: [PVar] -> UVarGenerator -> (UVarGenerator, PVar)
newVariable xs gen = let var = getFreshVar gen in
  if var `elem` xs
    then newVariable xs (getNextGen gen)
    else (gen, var)

-- | Generate a fresh variable
getFreshVar :: UVarGenerator -> PVar
getFreshVar gen = P . fresh $ gen ()

-- | Get the next generator
getNextGen :: UVarGenerator -> UVarGenerator
getNextGen gen = next $ gen ()

-- | Generate a fresh variable and the next generator (Pierce: 326)
varGen :: UVarGenerator
varGen = f 0
  where
    f :: Int -> UVarGenerator
    f n () = NextUVar ("p_" ++ show n) (f $ n + 1)

{--------------------------------------------------------------------------}
{- Unification -}

-- Cf. Hindley (1969), Damas & Milner (1982), and Pierce (2002: Ch.22).
-- We're going to use `unify` only for `condense`, whose input is two
-- propositional formulae. So, we don't bother with constraint sets.

-- | Determine if two formulae p and q are unifiable. If so, return their most
-- general unifier S, where S(p) === S(q) === C, their highest common instance
unify :: Prop -> Prop -> Subst
unify p q | p == q    = Map.empty
          | otherwise = case (p, q) of
            (a :-> b, a' :-> b') ->
              let s = unify a a' in
                let t = unify (apply s b) (apply s b') in
                  s ... t
            (Atom x, _) -> genSub x q
            (_, Atom x) -> genSub x p
  where
    genSub :: PVar -> Prop -> Subst
    genSub z t | z `Set.notMember` fv t = Map.fromList [(z, t)]
               | otherwise              = error $ "Couldn't unify " ++ name z ++ " and " ++ show t

{--------------------------------------------------------------------------}
{- Condensed Detachment -}

-- | Detach q from p to get the consequent of p
detach :: Prop -> Prop -> Either PropError Prop
detach p q = case p of
  Atom _    -> Left $ DErr p q
  p1 :-> p2 -> if p1 == q then Right p2 else Left $ DErr p q

-- | Condensed detachment (cf. HM, 1990: 94)
condense :: Prop -> Prop -> Prop
condense p q = case p of
  Atom _      -> p
  (p1 :-> p2) -> apply mgu p2
    where
      mgu = unify p1 q'
      q'  = apply (rename q p) q

-- Given a pair (p1 -> p2, q), we treat the equation `p1 === q` as a constraint
-- to solve using the unification algorithm. If there is an mgu S, then the
-- condensed detachment of these formulae is `S(p2)`, otherwise it is not defined.
