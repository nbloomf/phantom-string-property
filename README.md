phantom-string-property
=======================

Haskell library for cheap type-level encodings of string properties



What it is
----------

Types are great and all, but sometimes all we really want is a string.

Like

    "867-5309"

or

    "John, Paul, George, and Ringo"

or

    "<script>alert('pwned')</script>"

hey

    "../../../../../../../../../../../etc/passwd%00"

wait

    "'); DROP TABLE USERS; --"

no -- not like that!

Ok, sometimes all we really want is a string _that is guaranteed to satisfy some property_, like "doesn't contain unescaped HTML" or "isn't too long" or maybe even "is well-formed JSON" or "matches such-and-such regular expression".

There's a few ways to deal with this.

* Simple but fragile: have a predicate function that checks our string for the property we care about and use it before doing anything important.
* Robust but complicated: encode the underlying data structure using a type and parse the string into the type.

A well-known hybrid approach is to apply the fragile predicate function _once_, and if it succeeds encode this in the type system using a phantom type. This gives us the benefits of lightweight type safety without the burden of making parsers for every little thing.

This library provides a bunch of prefabricated property encodings.
