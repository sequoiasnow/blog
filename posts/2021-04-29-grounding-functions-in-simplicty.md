
---
title: Grounding Functions in Simplicity
tags: electricity, programming, minimalism
---

Recently I've had the privilege (also known as the mix of free time and a desire to try new ways of getting money) to work a few days as an electrician. There are several interesting things about this work, but one in particular has stood out to me: the design of the ground wire.

Now in the design of any home, one thing to avoid is it's potential destruction through an electrical fault. If a rat were to chew through a wire, or as is all the more likely in the bay, an earthquake destroy a cable, it would be highly undesirable for the resulting short to burn through wood and cause a fire, or electrocute someone opening the bridge say. To prevent this, most circuits come equipped with a ground wire, that is a wire that at some point is grounded in the Earth, which is such a large body that any unexpected discharge of electron in a grounded system would follow that wire back to Earth and be sucked back to harmless neutrality.

Now this is a reasonably simple concept, but there is the question of how to convey information about a circuit in a consistent manner. By custom, the "hot" wire, that is the wire of negative potential is colored black or red, and the "neutral" wire of more positive potential which receives the influx of electrons is marked as white. This makes a kind of sense, particularly as white is considered a base color in paintings, it's analog as neutral in electrical systems is obvious to anyone attempting to guess the nature of the given wire. However, how are we to indicate ground?

One approach would be to paint the wire **g**reen in reference to **g**round, but this might be confusing - after all none of the other wires are labeled according to mnemonic devices. In actual fact, the ground wire is indicated instead by it's purpose. Because it is the *ground* wire, it is the only wire which can always be touched without danger. Hence leaving the wire bare, instead of covered makes it's purpose immediate and obvious. It's function details it's purpose.

---

It seems to me that this notion of obvious functionality informing identity is of paramount importance in creating a maintainable code infrastructure. If we design a function with as minimal a set of input output parameters as are *functionally* necessary the nature of it's function becomes obvious without our needing to name it descriptively. It is so plain in it's type signature that we have no doubts about it's purpose or action.

Such signatures are eminent in simple functional code, consider the following examples

```haskell
x :: NonEmpty a -> a

y :: (KnownNat n, CmpNat i n ~ LT) => proxy i -> Vector n a -> a


z :: (Z f) => (a -> b) -> (c -> d) -> f a c -> f b d
```

Although there exists *some* ambiguity, the explicitly functional type signature gives an excellent indication of what each function does. The function `x` either takes the first or last element of a `NonEmpty` list. Since the only input given, `NonEmpty a` is guaranteed only to have at least one element, it has either a `head` or a `tail`, no other element can be guaranteed to exist. Hence, since we always receive an `a` from the function it must be one of these two. Were we to have written `NonEmpty a -> Maybe a` the ambiguity becomes much larger, since theoretically we could be operating on a potentially nonexistent element, such as the third of a list. By keeping the type small, we have narrowed it down to exactly two possibilities.


The function `y` quite obviously takes the `i`th element of a vector with strongly typed length `n` where `n > i`.  Again, no other possibility is permitted by the type signature, since we know nothing about `n` other than that it is greater than `i`, we do not even know if `n` is nonzero, the only possible value our code can retrieve is the `i`th index. The type signature completely determines the nature of the functions implementation.

Lastly `z` clearly takes a data type of kind `f :: * -> * -> *` and maps the first and second parameter. Indeed, this is simply the `bimap` function from prelude, but it is also eminently obvious what the `Z` constraint must indicate.

### In Practice

Now this is all well and good, and the prelude is littered with such examples of minimal type signatures. It's why [hoogle](hoogle.com) works and it's one of the great joys of writing Haskell programs. The problem is, that such attention is rarely given to internal functions pithing a function. It is all to common to write a function,


```haskell
validateUser :: User -> App (Maybe UserRegistrationErr)
```

which, while clear in it's intended usage, is utterly opaque in the manner of it's function. Does it access the database? Does it look at a list of common passwords? Does it do nothing at all, it's nigh on impossible to tell since the type signature is so ambiguous.

We can improve the situation somewhat by introducing constraints instead of using the nebulous `App` construct,


```haskell
validateUser
    :: ( Monad m
       , MonadIO m
       , Has HttpClient r
       , Has DBConnection r
       , MonadReader r m )
    => User
    -> App (Maybe UserRegistrationErr)
```

This makes it somewhat more clear what resources our code will use in order to validate the user, but it does not clarify exactly what shall be validated, or how. Are we performing checks of username length, or just uniqueness, are we looking at email validation or just leaving that field be? The problem, of course, is that this type of code is convenient, and, of course, we can always read through the documentation to find out what's going on. But, that documentation might not always be there, or up-to-date, and when the function does fail, it's entirely difficult to figure out at what stage it failed. Certainly the implementation of our function is not obvious from the type signature alone, but could we perhaps make it so?

Consider an alternative, turning the function around, we can make explicit which parameters are being checked.


```haskell
validateUser
    :: ( Monad m
       , MonadIO m
       , Has HttpClient r
       , Has DBConnection r
       , MonadReader r m )
    => UserRegistrationErr
    -> User
    -> m Bool
```

Writing the function in this manner, it's explicit which potential error we are testing against. Although there is still ambiguity as to the manner of each test, it has become entirely explicit what exactly we are testing, and the resulting type `m Bool` is no longer ambiguous, the test has passed or it has not.

Of course, this isn't perfect. Doubtless there are many ways to improve upon the function, we could establish explicit checks for each field in the user record and expose it only upon completion of those checks, use heterogeneous types to make the kinds of errors explicit, etc... However, the incremental improvement towards code with obvious functionality is important. Not only does it ease library maintenance by making a functions purpose and behavior obvious from the type signature, but by thus constraining the potential implementation of a function, we also constrain the potential for an unforeseen error therein.
