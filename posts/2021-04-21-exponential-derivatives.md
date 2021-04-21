---
title: Derivative's of Exponent's
tags: math, calculus
---

Let's start by taking the derivative of $e^x$. There are a few ways to do this,
some as simple as using Euler's identity, but perhaps the easiest is by way of
the chain rule. To do this we first need the derivative of $\ln(x)$. Recall
that

$$ e = \lim_{h\to 0} \frac{1}{(1+h)^h} $$

and hence for any $x$ we can write

$$
e = \lim_{ h / x \to 0} \frac{1}{\Big(1+\frac{h}{x}\Big)^{h/x}}
  = \lim_{ h / x \to 0} \Big ( 1 + \frac{h}{x}\Big )^\frac{x}{h}
$$

as a change of variables. Raising both sides to the $1/x$ we get that

$$
e^{\frac{1}{x}}
= \lim_{h/x \to 0} \Big( 1 + \frac{h}{x} \Big)^\frac{1}{h}
$$

Now, we can plug this into the definition of the derivative for logarithm, and
see that

$$
\begin{align*}
\frac{d}{dx} \ln(x) &= \lim_{h \to 0} \frac{\ln (x + h) - \ln(x)}{h} \\
&= \lim_{h\to 0} \frac{\ln\Big(\frac{x+h}{x}\Big)}{h} \\
&= \lim {h\to 0} \frac{\ln\Big(1 + \frac{h}{x}\Big)}{h} \\
&= \lim_{h\to 0} \frac{\ln\big(e^\frac{h}{x}\big)}{h} \\
&= \frac{1}{x}
\end{align*}
$$

It then becomes easy to find the derivative of $f(x) = e^x$ as well, since ${\ln\big(e^x\big)}
= x$, we have by the chain rule

$$
\frac{d}{dx} \ln(e^x) = \frac{1}{e^x} (e^x)' = 1
$$
4
Thus, clearly by multiplying: $\frac{d}{dx} e^x = e^x$.

## Other Exponents

Now, let's consider a more generic context. Let $a$ be a constant, we can find
the derivative of $a^x$ by use of the previous derivatives. Namely

$$
\begin{align*}
\frac{d}{dx} a^x &= \frac{d}{dx} e^{\ln\big(a^x\big)} \\
&= \frac{d}{dx} e^{x\ln(a)} \\
&= \ln(a) e^{x\ln(a)} \\
&= \ln(a) a^x.
\end{align*}
$$
