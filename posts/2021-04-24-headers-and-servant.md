---
title: Headers & Servant
tags: servant, haskell, programming
---

Although by it's very nature Servant's types give us an excellent indication of what routes are doing, it can at times be tricky to try and interact with. Particularly in the case of Headers, where competing approaches from [servant-auth](https://hackage.haskell.org/package/servant-auth) muddle the waters with Servant's inherent definitions.

Fortunately, though it does require some digging, all information necessary is provided by the Servant type system. Let's start by looking at the definition of a header as provided by Servant.

```haskell
data Header' (mods :: [*]) (sym :: Symbol) (a :: *)

type Header = Header' '[Optional, Strict]
```

As we can see, Servant provides two types. By default a `Header` is optional and can `Strict`, which is to say it's contents must be acceptably parsed via `FromHttpApiData`. However, as we can see it's also possible to specify other combinators for a `Header`, for instance making it `Required`. Then we come to the rest of the signature `sym :: Symbol` indicates the name of the `Header`, for instance `Authorization` and `a :: *` is the type of Header's contents.

Let's lookup a few examples and see how changing the `Header`s affects our handling functions


```haskell
-- Parse the "X-Example" header as Text, will run even if not present.
type APIExample1 =  Header "X-Example" Text :> Get '[JSON] JSON

example1 :: Server APIExample1
example1 = handler
    where
        handler :: Maybe Text -> Object
        handler = undefined


-- Require the "X-Example" header to be parsed as Text
type APIExample2 =  Header' '[Required, Strict] "X-Example" Text :> Get '[JSON] JSON

example2 :: Server APIExample1
example2 = handler
    where
        handler :: Text -> Object
        handler = undefined
```

So far we've been looking at the request side of Headers, which is pretty simple to parse, however, on the request side things get a bit more difficult. Now from a type perspective things remain rather simple, an endpoint which set's the `X-Example` header can be described in an obvious way.

```haskell

type APIExample3 :: Get '[JSON] (Headers '[Header "X-Example" Text] JSON)
```

However, things get a bit more complicated when it comes to actually setting the Header in our server. The documentation of `Servant.API.ResponseHeaders` is somewhat confusing since the larger part of it deals with presenting response headers in a type safe way using a heterogeneous list. Consider the `addHeader` function. It seemingly does what we want but it's type is a bit ambiguous.

```haskell
addHeader :: AddHeader h v orig new => v -> orig -> new
```

If we examine the `AddHeader` class things don't become any clearer, in fact the larger part of the definition of the class is dedicated to specifying the [Functional Dependencies](https://wiki.haskell.org/Functional_dependencies) which ensure that we don't get improper type overlaps. However, when we come to the instances we see something interesting.

```haskell
instance {-# OVERLAPPING #-} ( KnownSymbol h, ToHttpApiData v )
         => AddHeader h v (Headers (fst ': rest)  a) (Headers (Header h v  ': fst ': rest) a) where
    addOptionalHeader hdr (Headers resp heads) = Headers resp (HCons hdr heads)

instance {-# OVERLAPPABLE #-} ( KnownSymbol h, ToHttpApiData v
                       , new ~ (Headers '[Header h v] a) )
         => AddHeader h v a new where
    addOptionalHeader hdr resp = Headers resp (HCons hdr HNil)

```
