---
title: A Small Note on Big O Notation
tags: irony, programming
---

Big O notation is ubiquitous in programming, or at least that's the impression anyone in an introductory CS class would get. However, the truth of the matter is that for developers involved more in the creation of code than the development of higher theory it rarely occurs except in extreme edge cases concerning performance. The reason for this is simple, in that most developers make the correct choice to use existing and proven algorithms rather than developing their own, likely less performant, and certainly less maintainable, versions.

Still, rare as it is, at times it can be useful to recall the definition, and this post aims to serve as that reminder. Let us begin in mathematics where O notation was originally developed for the purposes of analysis. The definition reads as follows.

Suppose $f(x)$ and $g(x)$ are real functions, such that $g(x)$ is nonzero about $a \in \R$ and

$$
\limsup_{x \to a} \frac{|f(x)|}{|g(x)|} < \infty
$$

then we say that $f(x) = O(g(x)) where x \to a$.


> The above definition extends logically to other Metric Spaces.

The use of this notation is prevalent in the series expansion of functions where big O notation indicates the severity of the error term. Writing

$$
\sin(x) = x + O(x^3), \qquad x \to 0
$$

makes it clear that errors in the above approximation will be less than the order of $x^3$.

In the case of programming we can pair this down somewhat. Namely, since we are using O notation to give an indication of how many computations are performed for an algorithm given an input, we know several things. First our functions are discrete, and since computation cost can only count up, they are positive. Moreover, since we are considering the growth of performance under repeated calls, we take $x \to \infty$ by default. (Moreover since $x$ is discrete we adopt the standard notation for when $x$ is a natural number and name it $n$). In this context we can reduce the above definition to the following, by recalling the definition of a limit.


We say $f(x) = O(g(x))$ iff there exists some $N, C \in \mathbb{R}_+$ such that for all $x > N$,

$$
f(x) \leq Cg(x), \qquad \forall x > N.
$$


Applying this let's look at a few examples in code. A function which is simply a sequence of statements, takes the sum of those statements, hence in O notation, we care only about the statement with the maximum complexity, since all others will grow slower than that. Consider the case of loops, in a program which executes

```
for I in 1..N do
    for J in 1..M do
        simple computation
    end
end
```

we can see that the time complexity of the outer loop is $O(N *$ complexity of inner loop$)$. And the complexity of the inner loop is $O(M)$ so the total is $O(NM)$.

A more complex example might be binary search (which hunts down the position of x in a sorted array of length n):

```
function binary_search(array, n, x) do
    L = 0
    R = n - 1
    while L <= R do
        MID = floor( (L + R) / 2)
        if A[MID] < x do
           L = MID + 1
        else if A[MID] > x do
            R = MID
        else
            return A[MID]
        end
    end
end
```

Let's look at the complexity of the function. Assuming the worst case scenario (i.e. we only find the element at the last stage, where our array is of length 1) we see that during the first step the array is of length $n$, at the second $n/2$, the fourth $n/4$, etc... Since the algorithm continues iterating until there is one element, in this assumed worst case, the number of steps is then $k$ where

$$
\frac{n}{2^k} \leq 1
$$

If we simplify using $\log_2$ we find that

$$
n \leq 2^k  \\
\log_2(n) \leq k
$$

Hence the complexity is at most

$$ \lfloor \log_2(n) + 1 \rfloor $$

(here we get rid of the $\leq$ through flooring and adding one).

---

There are other examples of a similar vein, but this type of analysis alone is enough to give good indication of both the theory and the calculation, at least for a quick refresher.
