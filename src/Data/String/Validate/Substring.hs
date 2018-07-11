{-# LANGUAGE DataKinds, KindSignatures #-}
module Data.String.Validate.Substring (
    IsExactly(..)
  , PrefixedBy(..)
  , InfixedBy(..)
  , SuffixedBy(..)
  , NotPrefixedBy(..)
  , NotInfixedBy(..)
  , NotSuffixedBy(..)
) where

import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Typeable
import Data.Proxy
import GHC.TypeLits

import Data.String.Validate.Class



data IsExactly (s :: Symbol) = IsExactly (Proxy s)

instance (KnownSymbol s) => StringProperty (IsExactly s) where
  validator (IsExactly proxy) str =
    let m = symbolVal proxy in
    if m == str
      then Right ()
      else
        let
          err = "Expected exactly '" ++ m ++ "'"
        in
          Left [validationError err []]



data PrefixedBy (s :: Symbol) = PrefixedBy (Proxy s)

instance (KnownSymbol s) => StringProperty (PrefixedBy s) where
  validator (PrefixedBy proxy) str =
    let m = symbolVal proxy in
    if isPrefixOf m str
      then Right ()
      else
        let
          err = "Expected prefix '" ++ m ++ "'"
        in
          Left [validationError err []]



data InfixedBy (s :: Symbol) = InfixedBy (Proxy s)

instance (KnownSymbol s) => StringProperty (InfixedBy s) where
  validator (InfixedBy proxy) str =
    let m = symbolVal proxy in
    if isInfixOf m str
      then Right ()
      else
        let
          err = "Expected infix '" ++ m ++ "'"
        in
          Left [validationError err []]



data SuffixedBy (s :: Symbol) = SuffixedBy (Proxy s)

instance (KnownSymbol s) => StringProperty (SuffixedBy s) where
  validator (SuffixedBy proxy) str =
    let m = symbolVal proxy in
    if isSuffixOf m str
      then Right ()
      else
        let
          err = "Expected suffix '" ++ m ++ "'"
        in
          Left [validationError err []]



data IsNotExactly (s :: Symbol) = IsNotExactly (Proxy s)

instance (KnownSymbol s) => StringProperty (IsNotExactly s) where
  validator (IsNotExactly proxy) str =
    let m = symbolVal proxy in
    if m /= str
      then Right ()
      else
        let
          err = "Expected not exactly '" ++ m ++ "'"
        in
          Left [validationError err []]



data NotPrefixedBy (s :: Symbol) = NotPrefixedBy (Proxy s)

instance (KnownSymbol s) => StringProperty (NotPrefixedBy s) where
  validator (NotPrefixedBy proxy) str =
    let m = symbolVal proxy in
    if not $ isPrefixOf m str
      then Right ()
      else
        let
          err = "Did not expect prefix '" ++ m ++ "'"
        in
          Left [validationError err []]



data NotInfixedBy (s :: Symbol) = NotInfixedBy (Proxy s)

instance (KnownSymbol s) => StringProperty (NotInfixedBy s) where
  validator (NotInfixedBy proxy) str =
    let m = symbolVal proxy in
    if not $ isInfixOf m str
      then Right ()
      else
        let
          err = "Did not expect infix '" ++ m ++ "'"
        in
          Left [validationError err []]



data NotSuffixedBy (s :: Symbol) = NotSuffixedBy (Proxy s)

instance (KnownSymbol s) => StringProperty (NotSuffixedBy s) where
  validator (NotSuffixedBy proxy) str =
    let m = symbolVal proxy in
    if not $ isSuffixOf m str
      then Right ()
      else
        let
          err = "Did not expect suffix '" ++ m ++ "'"
        in
          Left [validationError err []]
