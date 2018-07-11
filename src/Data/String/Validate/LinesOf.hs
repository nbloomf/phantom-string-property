{-# LANGUAGE ScopedTypeVariables #-}
module Data.String.Validate.LinesOf (
    LinesOf(..)

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

import Data.String.Validate.Class



data LinesOf p = LinesOf p

instance (Typeable p, StringProperty p) => StringProperty (LinesOf p) where
  validator (LinesOf p) string =
    collectValidationErrors
      ("Lines of " ++ (show $ typeRep (Proxy :: Proxy p)))
      (zipWith (\i x -> ("Line " ++ show i, validator p x)) [1..] (lines string))



data LinesOf1 p1 = LinesOf1 p1

instance
  ( Typeable p1, StringProperty p1
  ) => StringProperty (LinesOf1 p1) where

  validator (LinesOf1 p1) str = case lines str of
    [l1] -> collectValidationErrors "Exactly Two Lines"
      [ ( "Line 1: " ++ (show $ typeRep (Proxy :: Proxy p1)), validator p1 l1 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 1"
      in
        Left [validationError err []]



data LinesOf2 p1 p2 = LinesOf2 p1 p2

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  ) => StringProperty (LinesOf2 p1 p2) where

  validator (LinesOf2 p1 p2) str = case lines str of
    [l1,l2] -> collectValidationErrors "Exactly Two Lines"
      [ ( "Line 1: " ++ (show $ typeRep (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep (Proxy :: Proxy p2)), validator p2 l2 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 2"
      in
        Left [validationError err []]



data LinesOf3 p1 p2 p3 = LinesOf3 p1 p2 p3

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  ) => StringProperty (LinesOf3 p1 p2 p3) where

  validator (LinesOf3 p1 p2 p3) str = case lines str of
    [l1,l2,l3] -> collectValidationErrors "Exactly Three Lines"
      [ ( "Line 1: " ++ (show $ typeRep (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep (Proxy :: Proxy p2)), validator p2 l2 )
      , ( "Line 3: " ++ (show $ typeRep (Proxy :: Proxy p3)), validator p3 l3 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 3"
      in
        Left [validationError err []]



data LinesOf4 p1 p2 p3 p4 = LinesOf4 p1 p2 p3 p4

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  ) => StringProperty (LinesOf4 p1 p2 p3 p4) where

  validator (LinesOf4 p1 p2 p3 p4) str = case lines str of
    [l1,l2,l3,l4] -> collectValidationErrors "Exactly Four Lines"
      [ ( "Line 1: " ++ (show $ typeRep (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep (Proxy :: Proxy p2)), validator p2 l2 )
      , ( "Line 3: " ++ (show $ typeRep (Proxy :: Proxy p3)), validator p3 l3 )
      , ( "Line 4: " ++ (show $ typeRep (Proxy :: Proxy p4)), validator p4 l4 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 4"
      in
        Left [validationError err []]



data LinesOf5 p1 p2 p3 p4 p5 = LinesOf5 p1 p2 p3 p4 p5

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  , Typeable p5, StringProperty p5
  ) => StringProperty (LinesOf5 p1 p2 p3 p4 p5) where

  validator (LinesOf5 p1 p2 p3 p4 p5) str = case lines str of
    [l1,l2,l3,l4,l5] -> collectValidationErrors "Exactly Five Lines"
      [ ( "Line 1: " ++ (show $ typeRep (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep (Proxy :: Proxy p2)), validator p2 l2 )
      , ( "Line 3: " ++ (show $ typeRep (Proxy :: Proxy p3)), validator p3 l3 )
      , ( "Line 4: " ++ (show $ typeRep (Proxy :: Proxy p4)), validator p4 l4 )
      , ( "Line 5: " ++ (show $ typeRep (Proxy :: Proxy p5)), validator p5 l5 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 5"
      in
        Left [validationError err []]



data LinesOf6 p1 p2 p3 p4 p5 p6 = LinesOf6 p1 p2 p3 p4 p5 p6

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
      [ ( "Line 1: " ++ (show $ typeRep (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep (Proxy :: Proxy p2)), validator p2 l2 )
      , ( "Line 3: " ++ (show $ typeRep (Proxy :: Proxy p3)), validator p3 l3 )
      , ( "Line 4: " ++ (show $ typeRep (Proxy :: Proxy p4)), validator p4 l4 )
      , ( "Line 5: " ++ (show $ typeRep (Proxy :: Proxy p5)), validator p5 l5 )
      , ( "Line 6: " ++ (show $ typeRep (Proxy :: Proxy p6)), validator p6 l6 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 6"
      in
        Left [validationError err []]



data LinesOf7 p1 p2 p3 p4 p5 p6 p7 = LinesOf7 p1 p2 p3 p4 p5 p6 p7

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
      [ ( "Line 1: " ++ (show $ typeRep (Proxy :: Proxy p1)), validator p1 l1 )
      , ( "Line 2: " ++ (show $ typeRep (Proxy :: Proxy p2)), validator p2 l2 )
      , ( "Line 3: " ++ (show $ typeRep (Proxy :: Proxy p3)), validator p3 l3 )
      , ( "Line 4: " ++ (show $ typeRep (Proxy :: Proxy p4)), validator p4 l4 )
      , ( "Line 5: " ++ (show $ typeRep (Proxy :: Proxy p5)), validator p5 l5 )
      , ( "Line 6: " ++ (show $ typeRep (Proxy :: Proxy p6)), validator p6 l6 )
      , ( "Line 7: " ++ (show $ typeRep (Proxy :: Proxy p7)), validator p7 l7 )
      ]

    ls ->
      let
        k = length ls
        err = "Got " ++ show k ++ " lines, but expected exactly 7"
      in
        Left [validationError err []]