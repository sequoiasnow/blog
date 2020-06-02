---
title: Prime Factorization
tags: math, prime numbers
---

Recently, I've been thinking about the disconnect between higher mathematics and
what the layman thinks of as math. After all, the further you study mathematics
the more you deal with abstract objects, proofs and logic - momentarily leaving
behind the realm of computation for more fundamental results. However, there is
an overlap, and one of the simplest examples I can give of that overlap are the
prime numbers.

An integer $n \in \mathbb{Z}$ is called prime if its only divisors are 1 and
itself. Written in logical notation we might say $n$ is prime when

$$
m | n \quad \implies \quad m = 1 \quad\text{or}\quad m = n.
$$

Prime numbers have been studied since antiquity, in because they are
inherently foundational to number theory.

Perhaps the most famous example is *the fundamental theorem of arithmetic* which
asserts that every integer $n$ is either 1 or can be expressed uniquely (up to sign) as a
product of prime numbers,

$$
n = p_1^{e_1} p_2^{e_2} \cdots p_k^{e_k}, \qquad
    e_i \in \mathbb{Z}_+, \text{$p$ prime}.
$$

Although the statement has far reaching implications it is actually quite simple
to prove - indeed, the first proof is attributed to Euclid although it may have
been known before him. The proof is inherently based upon the principal of
induction, which I will remark upon briefly before proceeding.

## Induction

> A well-known scientist (some say it was Bertrand Russell) once gave a public
> lecture on astronomy. He described how the earth orbits around the sun and how
> the sun, in turn, orbits around the centre of a vast collection of stars
> called our galaxy. At the end of the lecture, a little old lady at the back of
> the room got up and said: "What you have told us is rubbish. The world is
> really a flat plate supported on the back of a giant tortoise." The scientist
> gave a superior smile before replying, "What is the tortoise standing on?"
> "You're very clever, young man, very clever," said the old lady. "But it's
> turtles all the way down!"
>
> - Stephen Hawking, *A Brief History of Time*.


The key to induction is somewhat opposite the turtle principal - it is simply
one thing building on what is already true. Simply put suppose we have an
assertion $A(n)$ which we wish to verify for various cases $n = 1, 2, 3,
\dots$. Suppose we can show that $A(1)$ is true, how do we show the remaining
infinite cases?

Well since $A(1)$ is true, it follows that we can use $A(1)$ to prove
$A(2)$. Thus armed we can now prove $A(3)$ using $A(1)$ and $A(2)$. Indeed,
continuing in this manner, we see it suffices to prove that for any $n$ we can
prove assertion $A(n+1)$ given that $A(n), A(n-1), \dots A(1)$ are true.

The above is known colloquially as "strong induction" in reference to our
reliance upon all $1, \dots, n$ assertions. Induction which relies only upon the
$n$th assertion to prove the $n+1$th is typically called "weak" induction.

## Fundamental Theorem of Arithmetic

Let's get back to the proof. We first show the existence of such a factorization
by induction.

To apply induction we first have to consider the
base, or $n=1$ case. When $n=1$ then it is 1 and the assertion is proved. We now
consider a general number $n$, supposing that the assertion has been proved for
all $m < n$. If $n$ is prime it is already a product of primes, $n = n^1$ and we
are done. If not there exists some integers $1 < a < b$ such that $ab = n$. But
since $a <  b < n$, then the induction hypothesis applies and
$a = p_1^{e_1}p_2^{e_2}\cdots p_{k}^{e_k}$ and
$b = q_1^{h_1}q_2^{h_2}\cdots q_l^{h_l}$ which gives a prime factorization of
$n$, namely

$$
    n = ab = p_1^{e_1}p_2^{e_2}\cdots p_{k}^{e_k}
             q_1^{h_1}q_2^{h_2}\cdots q_l^{h_l} .
$$

The more interesting part of the proof lies in showing that this factorization
is in fact unique. What do we mean by unique? Basically that all $p_i$ and $e_i$
are unique, that any factorization consists of exactly one set of primes that
each appear exactly so many times. We prove this as follows.

Let $s$ be the smallest number with two distinct factorizations $s =
p_1p_2\cdots p_n$, $s = q_1 q_2 \cdots q_m$ (We know the exponents of the primes
to be 1 since if any of the $e_i$ were $> 1$ the number $s'$ made of their
product would be a smaller number with distinct factorizations).

First note that all the $p_i$ must be distinct from the $q_j$ since, were $p_i =
q_j$ then by cancellation $s / p_i = s/q_j$ would be a number $< s$ with
distinct factorizations, which violates our assumption that $s$ is the smallest
such number.

Suppose without
loss of generality that $q_1 > p_1$ (if not simply re-label that $q_i$ as $p_i$
and vice versa). Then let
$$ t = (q_1 - p_1)q_2q_3\cdots q_m. $$
Since $q_1 \neq p_1$ and $q_1 > p_1$, it's clear that $t > 0$. Moreover, since
$(q_1 - p_1)  < q_1$, we see that $t < s$ and hence must have a *unique*
factorization. Expanding,

$$
t = q_1q_2\cdots q_m - p_1(q_2\cdots q_m) = s - p_1(q_2 \cdots q_m) =
    p_1(p_2\cdots p_n - q_2 \cdots q_m).
$$

It's easy to see that since $t > 0$ and $p_1 > 0$, then
$s > p_2 \cdots p_n - q_2 \cdots q_m > 0$ so it has a unique factorization
which is a part of the factorization of $t$, hence $p_1$ is in the unique
factorization of $t$. Let $r_1\cdot r_k$ be the factorization of $q_1 - p_1$ so
that the unique factorization of $t$ is given as

$$ t = (r_1\cdots r_k) q_2 \cdots q_m . $$

Since we know $p_1$ is in that unique factorization, and
$p_1 \neq q_2 \neq \cdots q_m$
then $p_1$ is a factor of $q_1 - p_1$, that is there exists $r$ such
that $rp_1 = q_1 - p_1$ which suggests that $q_1 = (r + 1) p_1$ which
contradicts the assumption that $p_1$ is prime. Hence clearly this situation is
impossible, and hence the factorization of any integer is indeed unique.

---

The annoying thing about this proof is that it follows by contradiction. There
is an old saying that proofs by contradiction are easy to write but hard to
explain since they can delve into unrelated branches of mathematics in order to
show that the result is contradictory. In this case the contradiction is clear
to follow, but its difficult to see the clear chain of logic that led to the
result you might have in a more constructive proof. There are, however, various
proofs that do indeed provide a higher level proof of the above theorem. Let us
look at one of them.

## Properties of the Riemann-Zeta Function

Riemann Zeta function is a beloved part of mathematics, not least because it
continues to provide an active area of study in modern number theory. On first
encounter the function

$$
\zeta(s) = \sum_{n=1}^\infty \frac{1}{n^s}
$$

seems to have little to do with number theory, yet it has come to underpin many
of the most fascinating theories of the subject. In part, this is because we can
express the zeta function via Euler's product formula

$$
\zeta(s) = \prod_{\text{$p$ prime}} \frac{1}{1 - \frac{1}{p^s}}, \qquad \Re(s) > 1.
$$

To see the relation we have to introduce some more advanced theory, namely it's
a well known (and easy to prove!) result that for $\Re(s) > 1$

$$ \frac{1}{1 -p^{-s}} = 1 + p^{-s} + p^{-2s} + p^{-3s} + \cdots $$

Thus, if we take the product over all primes

$$ \prod_{p} \frac{1}{1-p^{-s}} = \sum_{n} \frac{F(n)}{n^s} $$

where $F(n)$ denotes the number of factorization of the integer $n$. To see this
note that we will have exactly $F(n)$ times that $1/n$ appears as a product of
distinct primes.

However, since we know that the Riemann zeta function is equal to Euler's
product formula we see that $F(n) \equiv 1$ for all $n$, hence every prime
factorization is unique.

Its relatively easy to see that the $\zeta$ function is indeed equal to Euler's
product formula via some analytic methods and we will do so in another post. For
now however, I think its fascinating how such a foundational result can be
encoded in such a strange thing as the zeta function. Yet perhaps it is not so
surprising, after all mathematics has always been bound up with the inexplicable
beauty connecting the abstract to the tangible.
