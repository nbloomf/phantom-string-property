{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Typeable
import Test.Tasty
import Test.Tasty.QuickCheck
import System.Environment (setEnv)

import Data.String.Validate



main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "4"
  defaultMain $
    localOption (QuickCheckTests 5000) $
    testGroup "Phantom String Properties"
      [ testValidatorFor
          DecimalDigits
          (all $ \c -> elem c "0123456789")

      , testValidatorFor
          LatinLowercase
          (all $ \c -> elem c "abcdefghijklmnopqrstuvwxyz")

      , testValidatorFor
          LatinUppercase
          (all $ \c -> elem c "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
      ]



testValidatorFor
  :: (Typeable p, StringProperty p)
  => p
  -> (String -> Bool)
  -> TestTree
testValidatorFor p q =
  let
    proxy :: p -> Proxy p
    proxy _ = Proxy
  in
    testProperty (show $ typeRep $ proxy p) $
      testValidator p q

testValidator
  :: (StringProperty p)
  => p
  -> (String -> Bool)
  -> String
  -> Bool
testValidator p q string =
  (q string) == (validator p string == Right ())
