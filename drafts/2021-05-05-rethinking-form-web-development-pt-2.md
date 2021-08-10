---
title: Rethinking Form Web Development Pt II: Attack of The Haskell
tags: haskell, web, form
---

In the [previous post](../2021-05-04-rethinking-form-web-development/) I talked,
at length (and only rarely with cohesion) about the benefits and perils of
reverting to forms as a method for developing web application. Now, because this
method of design has been around for quite some time, there are countless web
frameworks which work exceedingly well with that type of design pattern. PHP was
effectively built around it, ruby on rails remains incredibly popular, and
nearly every language has some hallowed framework which was originally built
upon this principle. But what of Haskell?

Well typical to Haskell, the origin of the predominant approach lies in [a
scientific paper](https://www.cl.cam.ac.uk/~jdy22/papers/formlets.pdf). The idea
of this paper is to create an abstraction over a form called a `Formlet` which
is:

- Composable: It an be combined with other Formlet's in order to make a larger
  form.
- Formlets are inherently associated with their actual representations, meaning
  that the form data received in posts and the html to render the form is always
  in sync.
- The incoming data can be parsed and validated. This, combined with the
  composable nature of a Formlet allows type safe forms to be built up easily.

Let's look at some of the examples given in the paper,

> The following is a somewhat more Haskell-like version of the existing
> pseudocode. It is notably changed for reasons we will see later on...

```haskell
makeDate :: Int -> Int -> Date
makeDate = undefined

dateFormlet :: Formlet Date
dateFormlet = liftA2 makeDate $
    [F|<div>
         Month: {{InputInt}}
         Day: {{InputInt}}
       </div>|]
```

Clearly the `dateFormlet` fulfills the criteria laid out for a Formlet. It is a
self contained way to yield a `Date`, and to do this it transforms the raw input
using the `makeDate` function. What's particularly interesting is the way we
transform the Formlet we created via
[QuasiQuote](https://wiki.haskell.org/Quasiquotation), which was of type
`Formlet Int -> Int` into our desired type `Formlet Date`. We simply use the
`Applicative` instance for `Formlet` to modify it's return type.

Moreover, note that manner in which we embed input's of type `Int`. These are
themselves `Formelt`s and hence, we see within this an embedding.

```haskell
Formlet a -> Formlet b -> Formlet (a -> b)
```

This might look familiar, and in fact is simply the exact opposite of
Applicative's `<$> :: f (a -> b) -> f a -> f b` embedding! What we see is a way
to construct a Formlet that is an exact reflection of the `Applicative` type
which we can then use to deconstruct it as we did with the `makeDate` function.

We then know, that to make our `Formlet` type it needs to in some way allow this
embedding action, as well be an `Applicative` type. Let's look at the
requirements together:

#### Renderable

Somehow, the Formlet must be "renderable", i.e. we must be able to display it as
`HTML` somehow.

```haskell
renderFormlet :: Formlet a -> HTML
```

#### Parsable

In order to render a Formlet, we also need a way to give names to inputs,
moreover this must be done in a deterministic way, since we also need to be able
to parse those inputs from an incoming post request. Let's call that
deterministic tree a `Legend`

```haskell
renderLegend :: Formlet a -> Legend a
```

We also declare a parsing function for incoming requests:

```haskell
parseFormlet :: HashMap Text Text -> Formlet a -> Maybe a
```

#### Embedable

One Formlet must be easily embed able within another, as we saw in the first
example. It must be embedable anywhere within as a child element, or a sibling
element, and it's label must be appropriately modified in accordance to that
idea.

```haskell
embed :: Formlet a -> Formlet b -> Formlet (a -> b)
```

#### Putting It Together

So, now that we have the requirements put together, how do we collect them into
a cohesive structure? Well one, rather simple way, is to start off by making a
structure that satisfies our `Applicative` requirement, and from there expand it
as needed until we have a complete type. To do that we need a way to embed a
type into a Formlet, `a -> Formlet a` and a way to sequential application,
`Formlet (a -> b) -> Formlet a -> Formlet b`

```haskell
data Formlet a where
    PureF  :: a -> Formlet a
    AppF :: Formlet (a -> b) -> Formlet a -> Formlet b
```

> Here we are using GADT's syntax, which allows us some more explicit notation
> in how we construct our types.


We can now construct an applicative instance.

```haskell
instance Applicative Formlet where
    pure = PureF
    (<*>) = AppF
```

We also need a `Functor` Instance, but it's easy to see that `fmap :: (a -> b)
-> f a -> f b` is relatively easy to write in terms of our existing structure:

```haskell
instance Functor Formlet where
    fmap f x = pure f <*> x
```

We now have an applicative instance in place! However it's not particularly
useful, all we've done is wrapped the definition of `Applicative` into a
structure. Most notably, there is no connection between the view of a Formlet
and it's associated type.

Let's think about that relationship for a moment. The idea is to bind together
the view component of a Formlet with it's HTML component. We also want to keep
the notion of tag-able inputs, so that we can populate a form with entry's that
will simply be empty to start with.

One way to visualize this is through the simplest input, that of type text. In
order to render that input we need some kind of name passed to it, this will
both allow it to be processed from the form input in the future. Let's look at
is a bit more:


```haskell
inputText :: Formlet Text
inputText = \ label ->
    PureF [text|<input type="text" name="#{label}" />|] mempty
```

Written in this suggestible form, we introduce the idea of a label being passed
to a `Formlet` element and the encapsulate the Html along side it, i.e


```diff
-    PureF  :: a -> Formlet a
+    PureF  :: Text -> Html -> a -> Formlet a
```



```haskell
dateFormlet = liftA2 makeDate $
    [F|<div>
        Month: {{InputInt}}
        Day: {{InputInt}}
       </div>|]


inputIntOpt :: Formlet (Maybe Int)
inputIntOpt l Nothing = PureF
    [text|<input type="text" name="#{l}" />|] Nothing
inputIntOpt l (Just i) = PureF
    [text|<input type="text" name="#{l}" value="#{i} /|] Nothing

inputTextReq :: Formlet Int
inputTextReq


data Formlet a = PureF (Label -> a -> Html) -> Maybe a -> Formlet a
```


A formlet is something which can be run against an input, so at some level the
type of


```haskell
inputInt :: Int -> Formlet Int
dateFormelt :: Int -> Int -> Formlet Date

run :: SomeData a -> Formlet a -> (Html, Maybe a)
```

There's also the aspect of a Formlet being bound in Html,


```haskell
dateFormlet = liftA2 makeDate $
    FHTML "<div> Month: " <> inputInt <> FHTML "Day : " <> inputInt <> FHTML
    "</div>"


instance Semigroup Formlet where
    FHTML t <> FHTML s = FHTML t <> s
    x <> y = MonoidF x y
```
