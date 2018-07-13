{-# LANGUAGE BangPatterns #-}
module Data.String.Validate.Char (
  -- * Character Classes
    PrintableChars(..)
  , DecimalDigits(..)
  , HexDigits(..)
  , Letters(..)
  , Numbers(..)
  , Punctuation(..)
  , LowerCaseChars(..)
  , UpperCaseChars(..)
  , WhitespaceChars(..)
  , MarkChars(..)
  , SymbolChars(..)
  , SeparatorChars(..)
  , ControlChars(..)
  , AlphaNumericChars(..)
  , AsciiChars(..)
  , Latin1Chars(..)

  -- * Each Character
  , EachCharIs(..)
) where

import Data.Char
import Data.Typeable

import Data.String.Validate.Class



allChars
  :: String -- ^ Name for the character class; used in error messages
  -> (Char -> Bool) -- ^ Predicate for allowed characters
  -> String -- ^ String to be tested
  -> Either [ValidationError] ()
allChars expects q = mapM_ check . zip [1..] 
  where
    check :: (Int, Char) -> Either [ValidationError] ()
    check (i,c) = if q c
      then Right ()
      else
        let
          err = "Read '" ++ [c] ++ "' at position "
            ++ show i ++ " but expected " ++ expects
        in
          Left [validationError err []]



-- | Satisfies @all isPrint@
data PrintableChars = PrintableChars
  deriving (Eq, Show, Typeable)

instance StringProperty PrintableChars where
  validator PrintableChars =
    allChars "printable character" isPrint



-- | Satisfies @all isDigit@
data DecimalDigits = DecimalDigits
  deriving (Eq, Show, Typeable)

instance StringProperty DecimalDigits where
  validator DecimalDigits =
    allChars "decimal digit" isDigit



-- | Satisfies @all isHexDigit@
data HexDigits = HexDigits
  deriving (Eq, Show, Typeable)

instance StringProperty HexDigits where
  validator HexDigits =
    allChars "hexdecimal digit" isHexDigit



-- | Satisfies @all isLetter@
data Letters = Letters
  deriving (Eq, Show, Typeable)

instance StringProperty Letters where
  validator Letters =
    allChars "letter" isLetter



-- | Satisfies @all isLower@
data LowerCaseChars = LowerCaseChars
  deriving (Eq, Show, Typeable)

instance StringProperty LowerCaseChars where
  validator LowerCaseChars =
    allChars "lower case letter" isLower



-- | Satisfies @all isUpper@
data UpperCaseChars = UpperCaseChars
  deriving (Eq, Show, Typeable)

instance StringProperty UpperCaseChars where
  validator UpperCaseChars =
    allChars "upper case letter" isUpper



-- | Satisfies @all isSpace@
data WhitespaceChars = WhitespaceChars
  deriving (Eq, Show, Typeable)

instance StringProperty WhitespaceChars where
  validator WhitespaceChars =
    allChars "whitespace character" isSpace



-- | Satisfies @all isNumber@
data Numbers = Numbers
  deriving (Eq, Show, Typeable)

instance StringProperty Numbers where
  validator Numbers =
    allChars "number" isNumber



-- | Satisfies @all isPunctuation@
data Punctuation = Punctuation
  deriving (Eq, Show, Typeable)

instance StringProperty Punctuation where
  validator Punctuation =
    allChars "punctuation mark" isPunctuation



-- | Satisfies @all isMark@
data MarkChars = MarkChars
  deriving (Eq, Show, Typeable)

instance StringProperty MarkChars where
  validator MarkChars =
    allChars "mark character" isMark



-- | Satisfies @all isSymbol@
data SymbolChars = SymbolChars
  deriving (Eq, Show, Typeable)

instance StringProperty SymbolChars where
  validator SymbolChars =
    allChars "symbol character" isSymbol



-- | Satisfies @all isSeparator@
data SeparatorChars = SeparatorChars
  deriving (Eq, Show, Typeable)

instance StringProperty SeparatorChars where
  validator SeparatorChars =
    allChars "separator character" isSeparator



-- | Satisfies @all isAlphaNum@
data AlphaNumericChars = AlphaNumericChars
  deriving (Eq, Show, Typeable)

instance StringProperty AlphaNumericChars where
  validator AlphaNumericChars =
    allChars "alphanumeric character" isAlphaNum



-- | Satisfies @all isControl@
data ControlChars = ControlChars
  deriving (Eq, Show, Typeable)

instance StringProperty ControlChars where
  validator ControlChars =
    allChars "control character" isControl



-- | Satisfies @all isAscii@
data AsciiChars = AsciiChars
  deriving (Eq, Show, Typeable)

instance StringProperty AsciiChars where
  validator AsciiChars =
    allChars "ASCII character" isAscii



-- | Satisfies @all isLatin1@
data Latin1Chars = Latin1Chars
  deriving (Eq, Show, Typeable)

instance StringProperty Latin1Chars where
  validator Latin1Chars =
    allChars "Latin1 character" isLatin1



data EachCharIs p = EachCharIs p
 deriving (Eq, Show, Typeable)

instance (StringProperty p) => StringProperty (EachCharIs p) where
  validator (EachCharIs !p) string =
    collectValidationErrors "Each character" $
      zipWith (\i c -> ("At position " ++ show i, validator p [c])) [1..] string
