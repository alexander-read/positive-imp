{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

----------------------------------------------------------------------------
-- |
-- Module      : Language.State
-- Description : Value declarations for L->
--
-- This module defines global value declarations for L-> formulae.
-- This lets proofs in the REPL resemble C.A. Meredith's 'D' notation.
--
----------------------------------------------------------------------------
module Language.State
  ( Values (..)
  , Value (..)
  , LookupErr (..)
  , emptyVals
  , addVal
  , lookupVal
  ) where

import Language.Grammar ( Prop )

import qualified Data.IntMap.Strict as IntMap

-- | A value declaration is a name for a formula
data Value = Val { ident :: Int, formula :: Prop } deriving Show

-- | A map of Values to manage the global state
newtype Values = Values { vals :: IntMap.IntMap Prop }

newtype LookupErr = LookupErr { message :: String }

-- | An initial empty global state
emptyVals :: Values
emptyVals = Values IntMap.empty

-- | Add a value declaration to the global state
addVal :: Value -> Values -> Values
addVal (Val n p) (Values vals) = Values $ IntMap.insert n p vals

-- | Lookup a value declaration in the global state
lookupVal :: Int -> Values -> Either LookupErr Prop
lookupVal name (Values vals) = case IntMap.lookup name vals of
    Just ast  -> Right ast
    Nothing   -> Left err
      where
        err = LookupErr $ "Value not found: " ++ show name
