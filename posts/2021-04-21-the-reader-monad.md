---
title: The Reader Monad
tags: haskell, programming
---

One of the ubiquitous (and somewhat scary) types in Haskell is the Reader
monad. However, when we get to the core of it, it's not that scary or
intimidating at all.

The basic idea of the reader monad is to allow the carrying of some underlying
piece of information through each instance of a command. But how do we
accomplish this? Well let's start by defining a type class for what we wish to
accomplish, a monad equipped with an operation to retrieve some information from
it.

```haskell
class Monad m => MonadReader r m | m -> r where
    ask :: m r
```

> Note the notation `| m -> r`. This is part of the GHC [Functional
> Dependencies](https://wiki.haskell.org/Functional_dependencies) extension
> which specifies that the larger type `m` determines the type `r`.

Now, let's consider them most obvious example of a reader we can, that is a
wrapper around the simplest monad we can think of the `Identity` monad, and to
add some functionality for retrieving the necessary information `r`.

```haskell
newtype Reader r a = Reader { runReader :: r -> Identity a }

instance MonadReader r (Reader r) where
    ask = ReaderT return

instance Monad Reader where
    return a = Reader $ \ r -> return

    m >>= f = Reader $ \ r -> do
        a <- runReader r
        runReader (f a) r
```

in fact, since we are effectively wrapping the functionality of the `Identity`
monad in order to make the `Reader`'s monad instance, we can acheive the same
result with any ordinary monad as well.

```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```

However, let's look at that type signature again. Note that the core of the
Reader functionality is the function which takes `r` and returns something. By
returning a type of monad we get a monad instance for `Reader`, however, we can
still get the reader instance without returning anything! The idea here, is that
the partially applied function is in fact a Monad, and furthermore is an
archatypal monad reader.

```haskell
instance Monad ((->) r) where
    return :: a -> r -> a
    return = const

    (>>=) :: (r -> a) -> (a -> r -> b) -> r -> b
    f >>= k = \ r -> k (f r) r

instance MonadReader r ((->) r) where
    ask = id

```

Note the similarity to the core idea of the reader monad, in fact since the
`Identity` monad is effectively a newtype wrapper, as is our `Reader` type, the
above is the actually simplest implementation of a `Reader` - and exactly the
definition given by Lambda Calculus.
