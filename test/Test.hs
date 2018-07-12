{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Char
import Data.Typeable
import Test.Tasty
import Test.Tasty.QuickCheck
import System.Environment (setEnv)

import Data.String.Validate



main :: IO ()
main = do
  setEnv "TASTY_NUM_THREADS" "4"
  defaultMain $
    localOption (QuickCheckTests 50000) $
    testGroup "Phantom String Properties"
      [ testGroup "Character Classes"
        [ testValidatorFor Letters (all isLetter)
        , testValidatorFor Numbers (all isNumber)
        , testValidatorFor Punctuation (all isPunctuation)
        , testValidatorFor MarkChars (all isMark)
        , testValidatorFor DecimalDigits (all isDigit)
        , testValidatorFor HexDigits (all isHexDigit)
        , testValidatorFor LowerCaseChars (all isLower)
        , testValidatorFor UpperCaseChars (all isUpper)
        , testValidatorFor AlphaNumericChars (all isAlphaNum)
        , testValidatorFor AsciiChars (all isAscii)
        , testValidatorFor Latin1Chars (all isLatin1)
        ]
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
