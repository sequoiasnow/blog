---
title: The Infinite Library
tags: math, infinity
---

Arguably the strangest part of mathematics is the consideration of
infinity. There is a kind of mysticism concerning limits at infinity that is
left unadressed in an introductory calculus class where infinity is taken more
as a tool than a mathematical object.

One of the most intriguing expansions on the concept of infinite is the
differentiation of higher infinities. At first, this doesn't seem like it
should be possible, after all what is larger than infinity? The answer to that
question was provided by the ever insightful Georg Cantor who first pioneered
the idea. One of the ways his idea concerning uncountability can be realized is
the infinite library. 


## The Infinite Library

Consider a library of vast size. It is filled with shelves each packed with
books, each seemingly identical a hundred pages long. You walk in and open the
first book on the first shelf only to find it blank. On the second you find
printed a single `a`. On the third `aa`, on the fourth, `aaa`. Eventaully the
entire alphabet

```
abcdefghijklmnopqrstuvwxyz1234567890,?!'":;()/. 
```

is enumerated through in every possible combination that can be fit within the
1000 page book. Within this library there are more books than there are atoms 
in the universe. Over $7.9 \times 10^{262}$ volumes, that is 

```
79766443076872509863361000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
```

books. It's an unfathomable number, made more so by what this library would
contain. Within it you would find the first 100 pages of every work of
shakespear. You would find unpublished volumes never written. Within it would be
definitive proof or counterexample to the Riemann Hypothesis. All of human
knowledge that ever was or ever will be expressed within a hundred pages would
be at home in the library. 

Now imagine that each book within the library were infinite, wouldn't the
library containing all such infinite books be something even larger? It turns
out we can show this.

## The Math

The standard form of infinity we are used to is called "countably"
infinite. It's analog is an infinite book, where ever character can be put in a
line and enumerated (in mathematical terms any set which can be surjectivly
projected onto by the natural numbers). Does this library conform to this same
notion of infinity? 

Suppose the books in the library could be enumerated as $B_1, B_2, B_3, \dots$
and consider each book as an infinite sequence of characters, i.e 

$$B_i = ldsahfkjasdfbvkjbadcjkladfbgjkldafbgkfdhagkljdfkjlbadkfb...$$

Were the library countable than every book within it could be enumerated in the
list. Our goal then, is to construct a book within the library that is outside
of any enumeration. This is commonly known as Cantor's Diagonalization
Argument and the origin of the notion of "uncountable." 

Suppose we consider the list of books as 

$$
\begin{align*}
    B_1 &= asdfbadf\dots \\
    B_2 &= lakjhdsf\dots \\
    B_3 &= ptsadfhg\dots \\
    B_4 &= hafgadjl\dots \\
    &\vdots
\end{align*}
$$

Then let us construct a new book, $B'$ as follows. Let the $i$th character of
$B'$ be the character following the $i$th character of $B_i$. That is, supose
the 2nd character of $B_2$ were $a$, then the second character of $B'$ would be
$b$, and so on. (Perhaps $z$ would map to $a$ or $.$ to $a$ if we allow special
characters in our book, then the cycle would repeat). In the case above we would
have 

$$ B' = bbth... $$

It's easy to see that for every $i$, the $i$th character of $B_i$ will be
distinct from the $i$th character of $B'$ by our construction. But this is
exactly the book we were looking for! Since $B'$ is distinct from every $B_i$ it
clearly isn't an element of the infinite sequence $B_1, B_2, B_3, \dots$ but
is just as clearly a book in its own right. Hence, the library we have described
can't be countable and we must seek out the new world **uncountable**. 

## Consequences 

The above diagonalization argument works for many sets, notably it is commonly
used to show that the real numbers are uncountable. Indeed, replace our books
with the decimal representations of real numbers and you get exactly the same
proof. However, it leads to further thoughts on infinity. Is there something
greater than uncountable? Well the same argument can be applied to ever greater
sets, building up an (ironically countable) set of infinities. Going further
there are even greater levels of infinity beyond the countable iterations, and
it's a fascinating topic that kept Georg Cantor up for much of his life. 

However, as soon as we hit uncountable things start to get interesting. Much of
the most interesting examples in calculus such as the Cantor set or the Devil's
staircase arise naturally as a consequence of the interplay between countable
and uncountable sets. 

