{-# LANGUAGE DataKinds, KindSignatures, BangPatterns #-}
module Data.String.Validate.FixedWidth (
  -- * Fixed Width
    ManyFixedWidth(..)
  , FixedWidth(..)

  -- * Fixed Format
  , FixedFormat1(..)
  , FixedFormat2(..)
  , FixedFormat3(..)
  , FixedFormat4(..)
  , FixedFormat5(..)
) where

import Data.Proxy
import Data.Typeable
import Data.List (genericSplitAt, genericLength, genericTake, intercalate)
import GHC.TypeLits

import Data.String.Validate.Class



data ManyFixedWidth (width :: Nat) p
  = ManyFixedWidth (Proxy width) p
  deriving (Eq, Show, Typeable)

instance (KnownNat n, StringProperty p) => StringProperty (ManyFixedWidth n p) where
  validator (ManyFixedWidth n p) str =
    let
      width = natVal $! n

      checkField :: Int -> String -> (String, Either [ValidationError] ())
      checkField i cs =
        let
          label = "Field " ++ show i

          checkWidth :: Either [ValidationError] ()
          checkWidth =
            let m = genericLength cs in
            if m == width
              then Right $! ()
              else
                let msg = "Length is " ++ show m ++ " but expected " ++ show width
                in Left [validationError msg []]
        in
          ( label
          , catValidationErrors
              [ checkWidth
              , validator p cs
              ]
          )

    in
      collectValidationErrors "Many fixed width fields" $
        zipWith checkField [1..] (chunksOf width str)





data FixedWidth (width :: Nat) (num :: Nat) p
  = FixedWidth (Proxy width) (Proxy num) p
  deriving (Eq, Show, Typeable)

instance (KnownNat n, KnownNat m, StringProperty p) => StringProperty (FixedWidth n m p) where
  validator (FixedWidth n m p) str =
    let
      width = natVal $! n
      num = natVal $! m

      fields = chunksOf width str

      checkNum :: (String, Either [ValidationError] ())
      checkNum =
        let
          w = genericLength fields
          label = "Number of fields is " ++ show w ++ " but expected " ++ show num
        in
          if w == num
            then (label, Right $! ())
            else (label, Left [])

      checkField :: Int -> String -> (String, Either [ValidationError] ())
      checkField i cs =
        let
          label = "Field " ++ show i

          checkWidth :: Either [ValidationError] ()
          checkWidth =
            let m = genericLength cs in
            if m == width
              then Right ()
              else
                let msg = "Length is " ++ show m ++ " but expected " ++ show width
                in Left [validationError msg []]
        in
          ( label
          , catValidationErrors
              [ checkWidth
              , validator p cs
              ]
          )

    in
      collectValidationErrors ("Fixed width fields (" ++ show num ++ ")") $
        checkNum : zipWith checkField [1..] (genericTake num fields)





data FixedFormat1
      (w1 :: Nat) p1
  = FixedFormat1
      (Proxy w1, p1)
  deriving (Eq, Show, Typeable)

instance
  ( KnownNat w1, StringProperty p1
  ) => StringProperty (FixedFormat1 w1 p1) where

  validator (FixedFormat1 (w1,p1)) str =
    let
      width1 = natVal $! w1

      widths = [width1]

      fields = chunksOf' widths str

    in
      collectValidationErrors ("One field of width " ++ commaList widths) $
        checkNum 1 fields : zipWith3 checkField [1] fields
          [ (width1, validator p1)
          ]





data FixedFormat2
      (w1 :: Nat) p1
      (w2 :: Nat) p2
  = FixedFormat2
      (Proxy w1, p1)
      (Proxy w2, p2)
  deriving (Eq, Show, Typeable)

instance
  ( KnownNat w1, StringProperty p1
  , KnownNat w2, StringProperty p2
  ) => StringProperty (FixedFormat2 w1 p1 w2 p2) where

  validator (FixedFormat2 (w1,p1) (w2,p2)) str =
    let
      width1 = natVal $! w1
      width2 = natVal $! w2

      widths = [width1, width2]

      fields = chunksOf' widths str

    in
      collectValidationErrors ("Two fields of width " ++ commaList widths) $
        checkNum 2 fields : zipWith3 checkField [1,2] fields
          [ (width1, validator p1)
          , (width2, validator p2)
          ]





data FixedFormat3
      (w1 :: Nat) p1
      (w2 :: Nat) p2
      (w3 :: Nat) p3
  = FixedFormat3
      (Proxy w1, p1)
      (Proxy w2, p2)
      (Proxy w3, p3)
  deriving (Eq, Show, Typeable)

instance
  ( KnownNat w1, StringProperty p1
  , KnownNat w2, StringProperty p2
  , KnownNat w3, StringProperty p3
  ) => StringProperty (FixedFormat3 w1 p1 w2 p2 w3 p3) where

  validator (FixedFormat3 (w1,p1) (w2,p2) (w3,p3)) str =
    let
      width1 = natVal $! w1
      width2 = natVal $! w2
      width3 = natVal $! w3

      widths = [width1, width2, width3]

      fields = chunksOf' widths str

    in
      collectValidationErrors ("Two fields of width " ++ commaList widths) $
        checkNum 3 fields : zipWith3 checkField [1,2,3] fields
          [ (width1, validator p1)
          , (width2, validator p2)
          , (width3, validator p3)
          ]





data FixedFormat4
      (w1 :: Nat) p1
      (w2 :: Nat) p2
      (w3 :: Nat) p3
      (w4 :: Nat) p4
  = FixedFormat4
      (Proxy w1, p1)
      (Proxy w2, p2)
      (Proxy w3, p3)
      (Proxy w4, p4)
  deriving (Eq, Show, Typeable)

instance
  ( KnownNat w1, StringProperty p1
  , KnownNat w2, StringProperty p2
  , KnownNat w3, StringProperty p3
  , KnownNat w4, StringProperty p4
  ) => StringProperty (FixedFormat4 w1 p1 w2 p2 w3 p3 w4 p4) where

  validator (FixedFormat4 (w1,p1) (w2,p2) (w3,p3) (w4,p4)) str =
    let
      width1 = natVal $! w1
      width2 = natVal $! w2
      width3 = natVal $! w3
      width4 = natVal $! w4

      widths = [width1, width2, width3, width4]

      fields = chunksOf' widths str

    in
      collectValidationErrors ("Two fields of width " ++ commaList widths) $
        checkNum 4 fields : zipWith3 checkField [1,2,3,4] fields
          [ (width1, validator p1)
          , (width2, validator p2)
          , (width3, validator p3)
          , (width4, validator p4)
          ]





data FixedFormat5
      (w1 :: Nat) p1
      (w2 :: Nat) p2
      (w3 :: Nat) p3
      (w4 :: Nat) p4
      (w5 :: Nat) p5
  = FixedFormat5
      (Proxy w1, p1)
      (Proxy w2, p2)
      (Proxy w3, p3)
      (Proxy w4, p4)
      (Proxy w5, p5)
  deriving (Eq, Show, Typeable)

instance
  ( KnownNat w1, StringProperty p1
  , KnownNat w2, StringProperty p2
  , KnownNat w3, StringProperty p3
  , KnownNat w4, StringProperty p4
  , KnownNat w5, StringProperty p5
  ) => StringProperty (FixedFormat5 w1 p1 w2 p2 w3 p3 w4 p4 w5 p5) where

  validator (FixedFormat5 (w1,p1) (w2,p2) (w3,p3) (w4,p4) (w5,p5)) str =
    let
      width1 = natVal $! w1
      width2 = natVal $! w2
      width3 = natVal $! w3
      width4 = natVal $! w4
      width5 = natVal $! w5

      widths = [width1, width2, width3, width4, width5]

      fields = chunksOf' widths str

    in
      collectValidationErrors ("Two fields of width " ++ commaList widths) $
        checkNum 5 fields : zipWith3 checkField [1,2,3,4,5] fields
          [ (width1, validator p1)
          , (width2, validator p2)
          , (width3, validator p3)
          , (width4, validator p4)
          , (width5, validator p5)
          ]





-- | Check number of fields with different lengths
checkNum :: Int -> [a] -> (String, Either [ValidationError] ())
checkNum i fields =
  case compare (genericLength fields) i of
    EQ -> ("", Right $! ())
    LT -> ("Expected " ++ show i ++ " field(s) of fixed width but missing data", Left [])
    GT -> ("Expected " ++ show i ++ " field(s) of fixed width but got extra data", Left [])

checkField
  :: Int -- ^ Field number
  -> String -- ^ Field text
  -> (Integer, String -> Either [ValidationError] ()) -- ^ Field width and validator
  -> (String, Either [ValidationError] ())
checkField i str (wid,v) =
  let
    m = genericLength str
    msg = "Expected length " ++ show wid ++ " but got " ++ show m
    checkWidth =
      if wid == m
        then Right $! ()
        else Left [validationError msg []]
  in
    ("Field " ++ show i, catValidationErrors [checkWidth, v str] )

chunksOf :: Integer -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs =
  let (w,ws) = genericSplitAt k xs in
  w : chunksOf k ws

chunksOf' :: [Integer] -> [a] -> [[a]]
chunksOf' ms as = case ms of
  [] -> [as]
  [k] -> chunksOf k as
  k:ks ->
    let (us,vs) = genericSplitAt k as in
    us : chunksOf' ks vs

commaList :: (Show a) => [a] -> String
commaList = intercalate "," . map show
