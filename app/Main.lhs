phantom-string-property
=======================

This is a literate source file; to follow along spin up a GHCi session with `stack ghci`.

Compiler noises:

> {-# LANGUAGE DataKinds #-}
> module Main where
> 
> import Data.Proxy
> import Data.String.Validate
> 
> main :: IO ()
> main = return ()



Getting Started
---------------

This package exposes four important pieces, reproduced here for reference:

```
newtype StringOf p = Valid String
  deriving (Eq, Show)

toString :: StringOf p -> String
toString (Valid s) = s

class StringProperty p where
  validator :: p -> String -> Either [ValidationError] ()

validate
  :: (StringProperty p)
  => p
  -> String
  -> Either [ValidationError] (StringOf p)
validate p s = case validator p s of
  Right () -> Right (Valid s)
  Left err -> Left err
```

Note that `StringOf p` type. The `p` parameter is what's sometimes called a _phantom type_, since it doesn't appear on the right hand side of the type definition. That, and the fact that `StringOf` is defined as a `newtype`, means that `p` plays no role whatsoever at runtime. Even the `Valid` constructor is optimized out.

What good is `p` then? We can use it to cheaply encode _properties_ at the type level. The constructor for `Valid p` is not exported, so the only way to construct a new `Valid p` value is via the `validate` function, which can only be used if `p` is an instance of the `StringProperty` class. In short -- a value of type `Valid p` can only exist if it satisfies the `validator p` function. Here's a simple example.

> data IsJustA = IsJustA
> 
> instance StringProperty IsJustA where
>   validator IsJustA string = if string == "A"
>     then Right ()
>     else Left [validationError "Expected just \"A\"" []]

The only way to construct a value of type `Valid IsJustA` is via the `validate` function with an `IsJustA` argument, and that function makes sure that any such value satisfies our `validator` predicate first. In this case, the predicate is `== "A"`.

We can test `validate` using the utility function `validateIO`, which pretty prints errors for us. Try this out in `ghci`:

```
$> validateIO IsJustA "A"
Valid "A"

$> validateIO IsJustA "B"
Validation Error!
Expected just "A"
```

Say we have a function that ostensibly takes a `String` argument, but really that function only makes sense for the string `"A"`. We can use the type system to enforce this restriction by taking not a `String`, but a `Valid IsJustA` -- which, recall, are indistinguishable at runtime.

> itsAnA :: StringOf IsJustA -> IO ()
> itsAnA x = if "A" == toString x
>   then putStrLn "Yup, it's an 'A' alright."
>   else putStrLn "This line never prints"

Now try this in `ghci`:

```
$> validateIO IsJustA "A" >>= itsAnA
Yup, it's an 'A' alright.

$> validateIO IsJustA "B" >>= itsAnA
Validation Error!
Expected just "A"
```

This kind of type safety is not quite as airtight as if we had full dependent types or subtyping; it depends on the fact that the constructor for `Valid p` is used exactly once, in the definition of `validate`. But it does have the advantage of simplicity, and sits between "arbitrary string" and "parsed data structure" on the power/safety spectrum.



The Built-In Properties
-----------------------

Phantom types with hidden constructors are a well-known technique for encoding constraints as types. There's even a popular library, [tagged](http://hackage.haskell.org/package/tagged), for using it with arbitrary types. What `phantom-string-properties` brings is a library of properties specific to `String` tested and ready to go.

Lets see some examples of increasing complexity. These examples all take a single `String` argument; try playing with them in `ghci`.

Character classes:

> ex1 = validateIO DecimalDigits
> 
> ex2 = validateIO LowerCaseChars
> 
> ex3 = validateIO UpperCaseChars

Multiple lines:

> ex4 = validateIO (ManyLinesOf DecimalDigits)
> 
> ex5 = validateIO (LinesOf2 DecimalDigits LowerCaseChars)

Length constraints (note the lower case):

> ex6 = validateIO lengthAtLeast7
> 
> ex7 = validateIO lengthAtMost9
> 
> ex8 = validateIO lengthIs5

The length constraint properties are implemented using type literals. More generally we can use any natural number for the constraint:

> ex9 = validateIO (LengthAtMost (Proxy :: Proxy 2048))

Note that this requires importing `Data.Proxy` and using the `DataKinds` extension. The library provides aliases (alii?) for these up to 128, which is why examples 6-8 use lower case constructors.

Substring constraints:

> ex10 = validateIO (PrefixedBy (Proxy :: Proxy "foo"))
> 
> ex11 = validateIO (NotSuffixedBy (Proxy :: Proxy "wut"))

These also require `Data.Proxy` and `DataKinds`.

Composite constraints:

> ex12 = validateIO (DecimalDigits, lengthIs10)

A tuple of `StringProperty`s is again a `StringProperty`, and the validator is the pointwise conjunction of the constituent properties.

> ex13 = validateIO (LinesOf2 (DecimalDigits, lengthAtMost5) LowerCaseChars)

We can have type-level regular expressions(!):

> ex14 = validateIO (Matches (Proxy :: Proxy "abc."))

Check the type of `ex14`:

```
$> :t ex14
ex14 :: String -> IO (Valid (Matches "abc."))
```

That regular expression is a _type constraint_, as in, we can use the type system to force strings to match it. I can't get over how cool that is. :)
