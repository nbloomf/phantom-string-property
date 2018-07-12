{-# LANGUAGE ScopedTypeVariables, LambdaCase, TypeOperators #-}
module Data.String.Validate.Class (
  -- * Validation
    Valid()
  , StringProperty(..)
  , toString
  , validate

  -- * Types
  , Or(..)

  -- * Errors
  , ValidationError
  , validationError
  , collectValidationErrors
  , validateIO
) where

import Data.Typeable
import Data.List (intersperse)
import System.Exit (exitFailure)



-- | `String` with a phantom /property type/ tag @p@. The constructor for `Valid` is not exported, and the only way to create a value of type `Valid p` is with `validate`.
newtype Valid p = Valid String
  deriving (Eq, Show)

-- | Recover the ordinary `String` value of a validated string.
toString :: Valid p -> String
toString (Valid s) = s

-- | Class for types representing properties that a string might satisfy, like having balanced parentheses or being valid HTML.
class StringProperty p where
  validator :: p -> String -> Either [ValidationError] ()

-- | Apply the validator
validate
  :: (StringProperty p)
  => p
  -> String
  -> Either [ValidationError] (Valid p)
validate p s = case validator p s of
  Right () -> Right (Valid s)
  Left err -> Left err



collectValidationErrors
  :: String
  -> [(String, Either [ValidationError] ())]
  -> Either [ValidationError] ()
collectValidationErrors title es = case collectLefts es of
  Right () -> Right ()
  Left err -> Left
    [ Tree title $ map (uncurry Tree) err ]

  where
    collectLefts :: [(a, Either b ())] -> Either [(a,b)] ()
    collectLefts list = case list of
      [] -> Right ()
      (a,e):rest -> case e of
        Right _ -> collectLefts rest
        Left b -> case collectLefts rest of
          Right _ -> Left [(a,b)]
          Left cs -> Left $ (a,b):cs



data Tree a
  = Tree a [Tree a]
  deriving (Eq, Show)

instance Functor Tree where
  fmap f (Tree x bs) = Tree (f x) (map (fmap f) bs)

prettyTree :: Tree String -> String
prettyTree = render . addPrefix
  where
    flatten :: Tree a -> [a]
    flatten (Tree x bs) = x : concatMap flatten bs

    render :: Tree String -> String
    render = concat . intersperse "\n" . flatten

    mapLast :: (a -> b) -> (a -> b) -> [a] -> [b]
    mapLast f g = \case
      []   -> []
      x:[] -> [g x]
      x:xs -> (f x) : mapLast f g xs

    mapRoot :: (a -> b) -> (a -> b) -> Tree a -> Tree b
    mapRoot f g (Tree x bs) = Tree (f x) (map (fmap g) bs)

    addPrefix :: Tree String -> Tree String
    addPrefix (Tree x bs) = Tree x $ mapLast
      (mapRoot ("├─ " ++) ("│  " ++))
      (mapRoot ("└─ " ++) ("   " ++))
      (map addPrefix bs)

type ValidationError = Tree String

-- | Constructor for validation errors
validationError :: String -> [ValidationError] -> ValidationError
validationError = Tree

prettyValidationErrors :: [ValidationError] -> String
prettyValidationErrors =
  concat . intersperse "\n" . map prettyTree



-- | Utility for testing `StringProperty` instances in ghci
validateIO
  :: (StringProperty p)
  => p
  -> String
  -> IO (Valid p)
validateIO p str = case validate p str of
  Right x -> return x
  Left err -> do
    putStrLn "\x1b[1;35mValidation Error!\x1b[0;39;49m"
    putStrLn $ prettyValidationErrors err
    exitFailure





data Or p q = p :|| q
  deriving (Eq, Show, Typeable)

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  ) => StringProperty (Or p1 p2) where

  validator (p1 :|| p2) str =
    case validator p1 str of
      Right () -> Right ()
      Left err1 -> case validator p2 str of
        Right () -> Right ()
        Left err2 -> Left
          [ validationError "In property disjunction"
            [ validationError (show $ typeRep (Proxy :: Proxy p1)) err1
            , validationError (show $ typeRep (Proxy :: Proxy p2)) err2
            ]
          ]



instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  ) => StringProperty (p1,p2) where

  validator (p1,p2) str =
    collectValidationErrors "In property conjunction"
      [ ( show $ typeRep (Proxy :: Proxy p1) , validator p1 str )
      , ( show $ typeRep (Proxy :: Proxy p2) , validator p2 str )
      ]

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  ) => StringProperty (p1,p2,p3) where

  validator (p1,p2,p3) str =
    collectValidationErrors "In property conjunction"
      [ ( show $ typeRep (Proxy :: Proxy p1) , validator p1 str )
      , ( show $ typeRep (Proxy :: Proxy p2) , validator p2 str )
      , ( show $ typeRep (Proxy :: Proxy p3) , validator p3 str )
      ]

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  ) => StringProperty (p1,p2,p3,p4) where

  validator (p1,p2,p3,p4) str =
    collectValidationErrors "In property conjunction"
      [ ( show $ typeRep (Proxy :: Proxy p1) , validator p1 str )
      , ( show $ typeRep (Proxy :: Proxy p2) , validator p2 str )
      , ( show $ typeRep (Proxy :: Proxy p3) , validator p3 str )
      , ( show $ typeRep (Proxy :: Proxy p4) , validator p4 str )
      ]

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  , Typeable p5, StringProperty p5
  ) => StringProperty (p1,p2,p3,p4,p5) where

  validator (p1,p2,p3,p4,p5) str =
    collectValidationErrors "In property conjunction"
      [ ( show $ typeRep (Proxy :: Proxy p1) , validator p1 str )
      , ( show $ typeRep (Proxy :: Proxy p2) , validator p2 str )
      , ( show $ typeRep (Proxy :: Proxy p3) , validator p3 str )
      , ( show $ typeRep (Proxy :: Proxy p4) , validator p4 str )
      , ( show $ typeRep (Proxy :: Proxy p5) , validator p5 str )
      ]

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  , Typeable p5, StringProperty p5
  , Typeable p6, StringProperty p6
  ) => StringProperty (p1,p2,p3,p4,p5,p6) where

  validator (p1,p2,p3,p4,p5,p6) str =
    collectValidationErrors "In property conjunction"
      [ ( show $ typeRep (Proxy :: Proxy p1) , validator p1 str )
      , ( show $ typeRep (Proxy :: Proxy p2) , validator p2 str )
      , ( show $ typeRep (Proxy :: Proxy p3) , validator p3 str )
      , ( show $ typeRep (Proxy :: Proxy p4) , validator p4 str )
      , ( show $ typeRep (Proxy :: Proxy p5) , validator p5 str )
      , ( show $ typeRep (Proxy :: Proxy p6) , validator p6 str )
      ]

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  , Typeable p5, StringProperty p5
  , Typeable p6, StringProperty p6
  , Typeable p7, StringProperty p7
  ) => StringProperty (p1,p2,p3,p4,p5,p6,p7) where

  validator (p1,p2,p3,p4,p5,p6,p7) str =
    collectValidationErrors "In property conjunction"
      [ ( show $ typeRep (Proxy :: Proxy p1) , validator p1 str )
      , ( show $ typeRep (Proxy :: Proxy p2) , validator p2 str )
      , ( show $ typeRep (Proxy :: Proxy p3) , validator p3 str )
      , ( show $ typeRep (Proxy :: Proxy p4) , validator p4 str )
      , ( show $ typeRep (Proxy :: Proxy p5) , validator p5 str )
      , ( show $ typeRep (Proxy :: Proxy p6) , validator p6 str )
      , ( show $ typeRep (Proxy :: Proxy p7) , validator p7 str )
      ]

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  , Typeable p5, StringProperty p5
  , Typeable p6, StringProperty p6
  , Typeable p7, StringProperty p7
  , Typeable p8, StringProperty p8
  ) => StringProperty (p1,p2,p3,p4,p5,p6,p7,p8) where

  validator (p1,p2,p3,p4,p5,p6,p7,p8) str =
    collectValidationErrors "In property conjunction"
      [ ( show $ typeRep (Proxy :: Proxy p1) , validator p1 str )
      , ( show $ typeRep (Proxy :: Proxy p2) , validator p2 str )
      , ( show $ typeRep (Proxy :: Proxy p3) , validator p3 str )
      , ( show $ typeRep (Proxy :: Proxy p4) , validator p4 str )
      , ( show $ typeRep (Proxy :: Proxy p5) , validator p5 str )
      , ( show $ typeRep (Proxy :: Proxy p6) , validator p6 str )
      , ( show $ typeRep (Proxy :: Proxy p7) , validator p7 str )
      , ( show $ typeRep (Proxy :: Proxy p8) , validator p8 str )
      ]

instance
  ( Typeable p1, StringProperty p1
  , Typeable p2, StringProperty p2
  , Typeable p3, StringProperty p3
  , Typeable p4, StringProperty p4
  , Typeable p5, StringProperty p5
  , Typeable p6, StringProperty p6
  , Typeable p7, StringProperty p7
  , Typeable p8, StringProperty p8
  , Typeable p9, StringProperty p9
  ) => StringProperty (p1,p2,p3,p4,p5,p6,p7,p8,p9) where

  validator (p1,p2,p3,p4,p5,p6,p7,p8,p9) str =
    collectValidationErrors "In property conjunction"
      [ ( show $ typeRep (Proxy :: Proxy p1) , validator p1 str )
      , ( show $ typeRep (Proxy :: Proxy p2) , validator p2 str )
      , ( show $ typeRep (Proxy :: Proxy p3) , validator p3 str )
      , ( show $ typeRep (Proxy :: Proxy p4) , validator p4 str )
      , ( show $ typeRep (Proxy :: Proxy p5) , validator p5 str )
      , ( show $ typeRep (Proxy :: Proxy p6) , validator p6 str )
      , ( show $ typeRep (Proxy :: Proxy p7) , validator p7 str )
      , ( show $ typeRep (Proxy :: Proxy p8) , validator p8 str )
      , ( show $ typeRep (Proxy :: Proxy p9) , validator p9 str )
      ]
