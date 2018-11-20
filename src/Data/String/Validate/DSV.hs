{-# LANGUAGE KindSignatures, DataKinds, TupleSections, ScopedTypeVariables #-}
module Data.String.Validate.DSV where

import Data.Typeable
import Data.Proxy
import Data.List (stripPrefix, unfoldr)
import GHC.TypeLits

import Data.String.Validate.Class



data ManySepBy (sep :: Symbol) (esc :: Symbol) p
  = ManySepBy (Proxy sep) (Proxy esc) p
  deriving (Eq, Show, Typeable)

instance (KnownSymbol sep, KnownSymbol esc, StringProperty p, Typeable p)
  => StringProperty (ManySepBy sep esc p) where
  validator (ManySepBy sepProxy escProxy p) string =
    let sep = symbolVal $! sepProxy in
    let esc = symbolVal $! escProxy in
    collectValidationErrors
      ("Many fields of " ++ (show $ typeRep $! (Proxy :: Proxy p))
        ++ " separated by '" ++ sep ++ "' and escaped by '" ++ esc ++ "'")
      (zipWith (\i x -> ("Field " ++ show i, validator p (concat x))) [1..] (splitEsc sep esc $ tokenize [esc,sep] string))



-- Given a _separator_ token and an _escape_ token,
-- split a list of tokens into fields. The first field
-- is the maximal prefix not containing a separator,
-- unless it is immediately preceded by an escape.
-- Note that the escape character has no significance
-- unless succeeded by a separator.
splitEsc :: (Eq a) => a -> a -> [a] -> [[a]]
splitEsc sep esc = unfoldr (nextSplit sep esc)
  where
    nextSplit :: (Eq a) => a -> a -> [a] -> Maybe ([a],[a])
    nextSplit sep esc str = if str == []
      then Nothing
      else Just $ accum [] str
        where
          accum stk zs = case (stk,zs) of
            (_,    [])  -> (reverse stk, [])
            ([],   v:vs) -> if v == sep
              then ([], vs)
              else accum [v] vs
            (u:us, v:vs) -> if v == sep
              then if u == esc
                then accum (v:us) vs
                else (u:us, vs)
              else accum (v:u:us) vs



-- Tokenize a string by taking the first prefix appearing in
-- a provided list of tokens, or just the first character if
-- none exists. The order on tokens is their ordinal index in
-- the input list, so if your code is not prefix-free just
-- make sure the token order is graded on length (decreasing).
tokenize :: (Eq a) => [[a]] -> [a] -> [[a]]
tokenize tokens = unfoldr (nextToken tokens)
  where
    stripToken :: (Eq a) => [a] -> [a] -> Maybe ([a],[a])
    stripToken token stream = case token of
      [] -> Nothing
      _ -> fmap (token,) $ stripPrefix token stream

    stripTokens :: (Eq a) => [[a]] -> [a] -> Maybe ([a],[a])
    stripTokens tokens stream = case tokens of
      [] -> Nothing
      t:ts -> case stripToken t stream of
        Nothing -> stripTokens ts stream
        Just x -> Just x

    nextToken :: (Eq a) => [[a]] -> [a] -> Maybe ([a],[a])
    nextToken tokens str = case str of
      [] -> Nothing
      c:cs -> case stripTokens tokens str of
        Just (as,bs) -> Just (as,bs)
        Nothing -> Just ([c],cs)
