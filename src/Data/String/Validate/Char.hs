module Data.String.Validate.Char (
  -- * Character Classes
    DecimalDigits(..)
  , HexDigits(..)
  , LatinLowercase(..)
  , LatinUppercase(..)

  -- * Utilities
  , allCharsIn
) where

import Data.Typeable

import Data.String.Validate.Class



-- | Utility for defining `StringProperty` instances that simply require all characters belong to a certain set.
allCharsIn
  :: String -- ^ Name for the character class; used in error messages
  -> [Char] -- ^ List of allowed characters
  -> String -- ^ String to be tested
  -> Either [ValidationError] ()
allCharsIn expects cs = mapM_ check . zip [1..] 
  where
    check :: (Int, Char) -> Either [ValidationError] ()
    check (i,c) = if elem c cs
      then Right ()
      else
        let
          err = "Read '" ++ [c] ++ "' at position "
            ++ show i ++ " but expected " ++ expects
        in
          Left [validationError err []]



data DecimalDigits = DecimalDigits
  deriving (Eq, Show, Typeable)

instance StringProperty DecimalDigits where
  validator DecimalDigits =
    allCharsIn "decimal digit" ['0'..'9']



data HexDigits = HexDigits
  deriving (Eq, Show, Typeable)

instance StringProperty HexDigits where
  validator HexDigits =
    allCharsIn "hexdecimal digit" $
      ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']



data LatinLowercase = LatinLowercase
  deriving (Eq, Show, Typeable)

instance StringProperty LatinLowercase where
  validator LatinLowercase =
    allCharsIn "lower case latin character" ['a'..'z']



data LatinUppercase = LatinUppercase
  deriving (Eq, Show, Typeable)

instance StringProperty LatinUppercase where
  validator LatinUppercase =
    allCharsIn "upper case latin character" ['A'..'Z']
