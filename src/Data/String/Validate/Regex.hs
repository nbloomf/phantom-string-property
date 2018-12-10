{-# LANGUAGE DataKinds, KindSignatures #-}

{-|
Module      : Data.String.Validate.Regex
Description : Regex predicates
Copyright   : (c) 2018 Automattic, Inc.
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
Portability : POSIX

Type-level constraints on regexes matching a string.
-}
module Data.String.Validate.Regex (
    Matches(..)
) where

import Data.Proxy
import Data.Typeable
import Text.Regex.PCRE ((=~))
import GHC.TypeLits

import Data.String.Validate.Class

-- | Satisfies @x =~ s@.
data Matches (s :: Symbol) = Matches (Proxy s)
  deriving (Eq, Show, Typeable)

instance (KnownSymbol s) => StringProperty (Matches s) where
  validator (Matches proxy) str =
    let m = symbolVal $! proxy in
    if str =~ m
      then Right ()
      else
        let
          err = "Expected string to match regex '" ++ m ++ "'"
        in
          Left [validationError err []]
