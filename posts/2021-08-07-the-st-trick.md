---
title: The ST Trick
tags: programming, types
---



Let's talk a about an issue which has plaugued many programers over the years: resource management. Particularly, the possibility of a mutable resource escaping the context it was designed for, and thus being unexpectedly altered in ways and lead to unexpected errors. At its core, the problem revolves around the possibility of a mutable resource to be unexpectedly altered by another thread. Consider the following contrived example whereby we create a mutable variable and then modify it with two different threads, 



```haskell
main :: IO () 
main = do
	let isAdmin = False
	ref <- newIORef isAdmin
	
	-- start a new thread which unexpectedly changes the value 
	thread <- forkIO $ modifyIORef ref not
	
	-- ...
	
	-- check for permissions 
	hasPriviliges <- readIORef ref 
	if hasPrivileges 
		-- ... working with an unexpected wrong value
	
```

The above example is quite contrived, but the issue it illustrates is a real one. Mutable variables by there very nature introduce a threat of inproper and unexpected modification because they are not bound to any particular thread of execution. It is not only the possibility of multi-threaded applications that creates this problem, a variable which leaks from a specified context can be problematic. Consider a session variable which escapes from a request context, this is an immediate security error that would not be caught by the type system. Fortunately, however, there is a way! 

Basically the idea is to **quarantine** a resource within a desired context, thus assuring it can not be used nor unexpectdly altered from without. We do this by making use of the so called [`ST` (state transformer) trick](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.50.3299&rep=rep1&type=pdf). 

Before introducing any trickery, let's start by considering the idea of a state transformer. We introduce a type `ST s a` which is a state transformer indexed by `s`. That is to say, `ST s a` holds within it a state `a` that is "named" or parametrized by a type `s`. The state monad `ST s a` holds a state indexed by `s` and delivering type `a`. The simplest example would simply be a state transformer which does nothing to the state at all, but simply returns the value, we'll call this `returnST :: a -> ST s a`. 

Now, in order to have any type of state within the transformer, we further need to define a type of mutable varaible which can hold the state. To do this we define a type `STRef s a`. Note that `STRef` is parameterized by `s` as well, thus the state is inherently bound to the same index. We define three functions for use with `STRef`'s:

```haskell
newSTRef :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a 
modfiySTRef :: STRef s a -> (a -> a) -> ST s ()
```

In order to use these functions together we need a way to sequence together various state transformers. Rather suggestively, we'll call this `bindST`.

```haskell
bindST :: ST s a -> (a -> ST s b) -> ST s b
```

Using `bindST` we can now sequence together multiple actions, each parametrized by the type variable `s`. It's easy to see that with `bindST` and `returnST` `ST s` can be made a monad (more details on the implementation below). It remains only to find a way to extract the computed state value from `ST s a`, we want a function that looks a little like the following:

```haskell
runST :: ST s a -> a 
```

However, we now have a problem. Using `runST` it is now possible to swap one variable from one `ST s` thread to another as follows:

```haskell
let var = runST (newSTRef "Hello World") in runST (readSTRef var) 
```

This is exactly the problem we were hoping to avoid! The problem lies in an examination of the type signature, let's look again at the type signature for `runST` this time writing the qualifiers explicitly. 

```haskell
runST :: forall s a. ST s a -> a 
```

Hence, in the above example, our function `runST` simply reuses the qualifier `s`. In order to explicitly speperate our contexts we need to ensure that the variable `s` does not escape our state transformer. Another way of thinking about this is that each use of `newSTRef` creates a new variable determiend by `s` in the thread and we wish for `runST` to have no knowledge of any initial variables in the thread as this could cause unexpected mutation. We do this by use of something called a rigid type variable, or as it's someone known a rigid skolem.



## Ahh, a Skolem!

 A skolem, a name which appears in several places in GHC compiler messages but does whose source is not quoted anywhere, is far less scary than it's name, or the associated mathematical concept. 

> In mathematics, quoth wikepedia, skolemization is the process of lifting existential quantifiers out of an expression and moving them before any universal quantifier. Usually this is done by introduction of a new function which depends only upon the preceding universal constants, and thus it's existence is equiviliant. I.e. $\forall x \exists y \forall z. Q(x,y,z)$ could be changed into skolem normal form by defining a new function $f$ such that $f(x) = y$ (this keeps the dependence of $y$ on $x$) and rewriting the expression as $\exists f \forall x \forall z. Q(x, f(x), z)$ which exists iff the original.  

In Haskell, it's much easier to understand. Consider again our definition of `runST :: ST s a -> a`, enabling `ExistentialQuantifiers` we have this explicitly as `runST :: forall s a. ST s a -> a`. The **trick** part of the ST trick, is to change the order of quantification by writing `runST` as  

```haskell
runST :: forall a. (forall s. ST s a) -> a
```

> This is what's known as a `Rank2Type` in Haskell, refering to the nested forall, and requires the `RankNTypes` extension.

What's going on here? Well, we are stating that `runST` must work for any `s` no matter the type. Because we explitly say that we accept any `s` for our type parameter, it can't leak. Let's look at that example which broke our quarantine origionally. 

```haskell
let var = runST (newSTRef "Hello World") in runST (readSTRef var) 
```

If we look at the type of `var` we can see that it's actually introduced two type variables! 

```haskell
runST (newSTRef "Hello World") :: (forall s. ST s (STRef s' String)) -> STRef s' String 
```

But this doesn't make any sense, there is no instance of `runST` that can be applied to `var` since the argument type doesn't match the *any* type of `s`. Indeed, GHC will complain if we attempt to run the above:

```
*Main> let var = runST (newSTRef "Hello World") in runST (readSTRef var) 

<interactive>:3:18: error:
    • Couldn't match type ‘a1’ with ‘STRef s [Char]’
        because type variable ‘s’ would escape its scope
      This (rigid, skolem) type variable is bound by
        a type expected by the context:
          forall s. ST s a1
        at <interactive>:3:17-40
      Expected type: ST s a1
        Actual type: ST s (STRef s [Char])
    • In the first argument of ‘runST’, namely
        ‘(newSTRef "Hello World")’
      In the expression: runST (newSTRef "Hello World")
      In an equation for ‘var’: var = runST (newSTRef "Hello World")
    • Relevant bindings include var :: a1 (bound at <interactive>:3:5)
```

What's preventing our `STRef` from escaping the context is of course the fact that `s` would no longer be contianed within the existential qualification. However, we can still use `ST` as normal, for instance, if we create a reference and then read it again we run into no issues, since the type we are returning is not bound to `s`. 

```haskell
runST $ do
	var <- newSTRef "Hello World"
	modifySTRef var $ flip (<>) "!"
	readSTRef var
	
-- "Hello World!"
```

There are [many other examples](https://wiki.haskell.org/Monad/ST) about how the trick allows using mutable logic in ways that a functional program would typically allow, **without** sacrificing type safety. However, to me the most impressive part of the `ST` trick is the fact that it isn't built out compiler level trickery, but rather entirely made possible by the Haskell type system. Moreover if we look a little more into the initial paper on the subject we see a rather interesting definition, one that is quite similar to GHC's own:

```haskell
type IO a = ST RealWorld a
```



---

### An Example

This is adapted from Sandy Magurie's _Thinking With Types_ which is an excellent book about higher order type level programming in Haskell. Note the use of `unsafePerformIO` in our implementation is not exposed, and hence can not lead to type errors (it is possible to rework the example without such reliance on )

```haskell
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module Lib
  ( ST
  , runST
  , STRef
  , newSTRef
  , writeSTRef
  , readSTRef
  , modifySTRef )
where

import           Data.IORef
import           GHC.IO     (unsafePerformIO)

newtype ST s a = ST
  { unsafeRunST :: a -- not exposed, but used under the hood
  }

-- actual execution, note the only difference from @unsafeRunST@ is the
-- universal qualifier indicating /any/ s.
runST :: (forall s. ST s a) -> a
runST = unsafeRunST

-- instances

instance Functor (ST s) where
  fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
  pure = ST
  (ST f) <*> (ST a) = seq a . ST $ f a

instance Monad (ST s) where
  (ST a) >>= f = seq a $ f a

-- working with st ref's

newtype STRef s a = STRef
  { unSTRef :: IORef a }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = readSTRef ref >>= (writeSTRef ref . f)

```





```haske
newtype STRef a = STRef (IORef a )
```

