---
title: A Modern Approach to Css In Haskell
tags: haskell, css, programming
---

Since I've been building this blog with [Hakyll](https://jaspervdj.be/hakyll/)
I've been attempting to use as much Haskell in its development as possible, both
because I think it is the best way to make the project more cohesive and also
because it's a lot of fun :-]. One of the early stumbling blocks I found was
integrate css into an otherwise haskell based project - even with a preprocessor
css is hardly functional. Fortunately there exist some libraries out there that
aim to solve exactly this problem. Foremost among them is
[Clay](http://fvisser.nl/clay), a monadic CSS preprocessor which I decided to
try out at once.

<!--more-->

CSS In clay looks something like the following,


```haskell
fancyButton :: Css
fancyButton = ".fancy-button" ?
  do backgroundColor   tomato
     sym2 padding      (em 0.5) (em 1)
     sym borderRadius  (px 5)
     color             white
     border            solid (px 1) red

     hover &
       do color        green
```

On the whole its quite accessible, and, after getting used to the way selectors
are applied, Clay is quite easy to use. However, there are limitations. For one,
it is difficult to provide any theme wrapper to the CSS monad, if we were so
inclined. For instance if we were to wrap the `Css` monad with a reader so that
we might access some theme object within our code:

```haskell
data Theme = Theme { bg_color :: Color }
type ThemeCss = ReaderT Theme Css ()
```

we immediately run into some problems. The previously easy syntax becomes
riddled with `lift`s and absolutely untenable. As we continue other roadblocks
make themselves manifest. The clay auto-prefixer for instance simply applies all
browser prefixes to a property,

```haskell
browsers :: Prefixed
browsers = Prefixed
  [ ( "-webkit-", "" )
  , (    "-moz-", "" )
  , (     "-ms-", "" )
  , (      "-o-", "" )
  , (         "", "" )
  ]
```

when in many cases the prefixes are less all encompassing. While hardly a huge
problem this still means the generated CSS is less predictable as well as
slightly larger.

## Can We do Better?

All this made me wonder if there were any way to improve upon the already great
work done in Clay? For instance it would be relatively simple to change the
types of the `Css` monad to a class instead so that we could wrap it in a monad
stack without worry. To that end, I decided to embark upon building an updated
version of Clay with the goal of including the following features.

### Allowing Monadic Theming

The goal here is to allow something like

```haskell
compileCss' (Theme { bg_color = red }) $ do
  h1 ?
    color   (t bg_color)
```

One of the notable benefits would be the ability to create light/dark mode
themes without needlessly duplicating code.

```haskell
compileCss $
    ".dark"  ? withTheme darkTheme  ccsM
    ".light" ? withTheme lightTheme cssM
```

Of course this might be made even easier by making the theme a `Semigroup`.

### Defining Modern CSS Rules

One of the current annoyances in Clay is the lack of any definitions for `grid-`
properties. A high priority would be the addition of modern CSS rules which are
becoming increasingly popular. For instance we might define
`grid-template-areas` in a strongly typed way.

### Allowing `var`

One of the great features of modern css has been css variables. Not only do
these simplify styling, but they are the core of dynamic themeing. If we are to
include themes in our css preprocessor it makes sense we should also include a
way to make them dynamic. To that end we want to create a way to enable use of
css variables, perhaps as extensions of themes. For instance,

```haskell
compileCss' (Theme { bg_color = red }) $ do
  h1 ?
    color   (t bg_color)
```

might render

```css
:root {
    --bg-color: red;
}

h1 {
  color: red;
  color: var(--bg-color);
}
```

The goal would be to bind the development patterns in our css pre-processor as
closely to modern css best practices as we can without sacrificing the benefits
of our strongly typed haskell programming.

### Include The Root Selector

One of the older, but still popular conventions for CSS class naming is the BEM
style, `.object__sub-object--modifier`. Sass makes this an easy format to use by
introducing the `&` selector which references the parent, a feature it seems
worthwhile to include as a feature in our own preprocessor.

### A Lack of CSS Duplication

Although it has very little impact on the actual rendering of CSS, the
duplication of styles in Clay is somewhat of an annoyance. Notably,

```haskell
color       red
color       green
```
is rendered as
```css
color: red;
color: green;
```
whereas it would seem a simple effort to reduce this to `color: green`. Notably
this allows extensions of mixin styles without unwanted duplication.

### Other Features?

There may well be other features to include as development goes on, but for the
moment we'll settle on achieving these goals without sacrificing any of the
incredible features already found in Clay. Over the course of the next few
weeks, I'll be posting updates on the development of this new system and the
pitfalls and benefits of building such a system.
