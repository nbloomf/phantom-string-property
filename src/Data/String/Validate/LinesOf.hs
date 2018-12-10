{-# LANGUAGE ScopedTypeVariables, DataKinds, KindSignatures, BangPatterns #-}

{-|
Module      : Data.String.Validate.LinesOf
Description : Line predicates
Copyright   : (c) 2018 Automattic, Inc.
License     : GPL-3
Maintainer  : nbloomf@gmail.com
Stability   : experimental
Portability : POSIX

Type-level constraints on the number and contents of the lines in a string.
-}
module Data.String.Validate.LinesOf (
    ManyLinesOf(..)
  , LinesOf(..)

  -- * Fixed Number of Lines
  , LinesOf1(..)
  , LinesOf2(..)
  , LinesOf3(..)
  , LinesOf4(..)
  , LinesOf5(..)
  , LinesOf6(..)
  , LinesOf7(..)
) where

import Data.Typeable
import GHC.TypeLits
import Data.List (genericTake, genericLength)

import Data.String.Validate.Class



-- | Each line of @x@ satisfies @p@.
data ManyLinesOf p = ManyLinesOf p
  deriving (Eq, Show, Typeable)

instance (Typeable p, StringProperty p) => StringProperty (ManyLinesOf p) where
  validator (ManyLinesOf p) string =
    collectValidationErrors
      ("Many lines of " ++ (show $ typeRep $! (Proxy :: Proxy p)))
      (zipWith (\i x -> ("Line " ++ show i, validator p x)) [1..] (lines string))



-- | Exactly @num@ lines, each satisfying @p@.
data LinesOf (num :: Nat) p = LinesOf (Proxy num) p
  deriving (Eq, Show, Typeable)

instance (KnownNat n, Typeable p, StringProperty p) => StringProperty (LinesOf n p) where
  validator (LinesOf n p) str =
    let
      num = natVal $! n

      lns = lines str

      checkNum :: (String, Either [ValidationError] ())
      checkNum =
        let
          w = genericLength lns
          label = "Number of lines is " ++ show w ++ " but expected " ++ show num
        in
          if w == num
            then ("", Right $! ())
            else (label, Left [])

      checkLine :: Int -> String -> (String, Either [ValidationError] ())
      checkLine i cs =
        ("Line " ++ show i, validator p cs )

    in
      collectValidationErrors ("Fixed number of lines (" ++ show num ++ ")") $
        checkNum : zipWith checkLine [1..] (genericTake num lns)



-- | Exactly one line satisfying @p1@.
data LinesOf1 p1 = LinesOf1 p1
  deriving (Eq, Show, Typeable)

instance
  ( Typeable p1, StringProperty p1
  ) => StringProperty (LinesOf1 p1) where

  validator (LinesOf1 p1) str = case lines str of
    [l1] -> collectValidationErrors "Exactly Two Lines"
      [ ( "Line 1: " ++ (show $ typeRep $! (Proxy :: Proxy p1)), validator p1 l1 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 1"
      in
        Left [validationError err []]



-- | Exactly two lines, satisfying @p1@ and @p2@, respectively.
data LinesOf2 p1 p2 = LinesOf2 p1 p2
  deriving (Eq, Show, Typeable)

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  ) => StringProperty (LinesOf2 p1 p2) where

  validator (LinesOf2 p1 p2) str = case lines str of
    [l1,l2] -> collectValidationErrors "Exactly Two Lines"
      [ ( "Line 1: " ++ (show $ typeRep $! (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep $! (Proxy :: Proxy p2)), validator p2 l2 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 2"
      in
        Left [validationError err []]



-- | Exactly three lines, satisfying @p1@, @p2@, and @p3@, respectively.
data LinesOf3 p1 p2 p3 = LinesOf3 p1 p2 p3
  deriving (Eq, Show, Typeable)

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  ) => StringProperty (LinesOf3 p1 p2 p3) where

  validator (LinesOf3 p1 p2 p3) str = case lines str of
    [l1,l2,l3] -> collectValidationErrors "Exactly Three Lines"
      [ ( "Line 1: " ++ (show $ typeRep $! (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep $! (Proxy :: Proxy p2)), validator p2 l2 )
      , ( "Line 3: " ++ (show $ typeRep $! (Proxy :: Proxy p3)), validator p3 l3 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 3"
      in
        Left [validationError err []]



-- | Exactly four lines, satisfying @p1@, @p2@, @p3@, and @p4@, respectively.
data LinesOf4 p1 p2 p3 p4 = LinesOf4 p1 p2 p3 p4
  deriving (Eq, Show, Typeable)

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  ) => StringProperty (LinesOf4 p1 p2 p3 p4) where

  validator (LinesOf4 p1 p2 p3 p4) str = case lines str of
    [l1,l2,l3,l4] -> collectValidationErrors "Exactly Four Lines"
      [ ( "Line 1: " ++ (show $ typeRep $! (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep $! (Proxy :: Proxy p2)), validator p2 l2 )
      , ( "Line 3: " ++ (show $ typeRep $! (Proxy :: Proxy p3)), validator p3 l3 )
      , ( "Line 4: " ++ (show $ typeRep $! (Proxy :: Proxy p4)), validator p4 l4 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 4"
      in
        Left [validationError err []]



-- | Exactly five lines, satisfying @p1@, @p2@, @p3@, @p4@, and @p5@, respectively.
data LinesOf5 p1 p2 p3 p4 p5 = LinesOf5 p1 p2 p3 p4 p5
  deriving (Eq, Show, Typeable)

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  , Typeable p5, StringProperty p5
  ) => StringProperty (LinesOf5 p1 p2 p3 p4 p5) where

  validator (LinesOf5 p1 p2 p3 p4 p5) str = case lines str of
    [l1,l2,l3,l4,l5] -> collectValidationErrors "Exactly Five Lines"
      [ ( "Line 1: " ++ (show $ typeRep $! (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep $! (Proxy :: Proxy p2)), validator p2 l2 )
      , ( "Line 3: " ++ (show $ typeRep $! (Proxy :: Proxy p3)), validator p3 l3 )
      , ( "Line 4: " ++ (show $ typeRep $! (Proxy :: Proxy p4)), validator p4 l4 )
      , ( "Line 5: " ++ (show $ typeRep $! (Proxy :: Proxy p5)), validator p5 l5 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 5"
      in
        Left [validationError err []]



-- | Exactly six lines, satisfying @p1@, @p2@, @p3@, @p4@, @p5@, and @p6@, respectively.
data LinesOf6 p1 p2 p3 p4 p5 p6 = LinesOf6 p1 p2 p3 p4 p5 p6
  deriving (Eq, Show, Typeable)

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  , Typeable p5, StringProperty p5
  , Typeable p6, StringProperty p6
  ) => StringProperty (LinesOf6 p1 p2 p3 p4 p5 p6) where

  validator (LinesOf6 p1 p2 p3 p4 p5 p6) str = case lines str of
    [l1,l2,l3,l4,l5,l6] -> collectValidationErrors "Exactly Six Lines"
      [ ( "Line 1: " ++ (show $ typeRep $! (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep $! (Proxy :: Proxy p2)), validator p2 l2 )
      , ( "Line 3: " ++ (show $ typeRep $! (Proxy :: Proxy p3)), validator p3 l3 )
      , ( "Line 4: " ++ (show $ typeRep $! (Proxy :: Proxy p4)), validator p4 l4 )
      , ( "Line 5: " ++ (show $ typeRep $! (Proxy :: Proxy p5)), validator p5 l5 )
      , ( "Line 6: " ++ (show $ typeRep $! (Proxy :: Proxy p6)), validator p6 l6 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 6"
      in
        Left [validationError err []]



-- | Exactly seven lines, satisfying @p1@, @p2@, @p3@, @p4@, @p5@, @p6@, and @p7@, respectively.
data LinesOf7 p1 p2 p3 p4 p5 p6 p7 = LinesOf7 p1 p2 p3 p4 p5 p6 p7
  deriving (Eq, Show, Typeable)

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  , Typeable p5, StringProperty p5
  , Typeable p6, StringProperty p6
  , Typeable p7, StringProperty p7
  ) => StringProperty (LinesOf7 p1 p2 p3 p4 p5 p6 p7) where

  validator (LinesOf7 p1 p2 p3 p4 p5 p6 p7) str = case lines str of
    [l1,l2,l3,l4,l5,l6,l7] -> collectValidationErrors "Exactly Seven Lines"
      [ ( "Line 1: " ++ (show $ typeRep $! (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep $! (Proxy :: Proxy p2)), validator p2 l2 )
      , ( "Line 3: " ++ (show $ typeRep $! (Proxy :: Proxy p3)), validator p3 l3 )
      , ( "Line 4: " ++ (show $ typeRep $! (Proxy :: Proxy p4)), validator p4 l4 )
      , ( "Line 5: " ++ (show $ typeRep $! (Proxy :: Proxy p5)), validator p5 l5 )
      , ( "Line 6: " ++ (show $ typeRep $! (Proxy :: Proxy p6)), validator p6 l6 )
      , ( "Line 7: " ++ (show $ typeRep $! (Proxy :: Proxy p7)), validator p7 l7 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 7"
      in
        Left [validationError err []]
