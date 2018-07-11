{-# LANGUAGE DataKinds, KindSignatures #-}
module Data.String.Validate.Regex (
    Matches(..)
) where

import Data.Proxy
import Text.Regex.PCRE ((=~))
import GHC.TypeLits

import Data.String.Validate.Class

data Matches (s :: Symbol) = Matches (Proxy s)

instance (KnownSymbol s) => StringProperty (Matches s) where
  validator (Matches proxy) str =
    let m = symbolVal proxy in
    if str =~ m
      then Right ()
      else
        let
          err = "Expected string to match regex '" ++ m ++ "'"
        in
          Left [validationError err []]
