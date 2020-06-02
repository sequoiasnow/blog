---
title: Testing What Needs To Be Tested
tags: programming, testing
---

One of the most important elements of quality software development is
testing. As much as we'd like to be able to write perfect code in one go, it's
rather difficult - in fact, it almost never happens. Even in the case where code
works as intended, there are almost always cases where the user thinks of some
clever way to mess up what should be perfectly suitable code. I recall writing a
regular expression for email that failed to catch addresses of the
`user+team@domain.com` variety - an error that was only caught after a complaint
was filed with the customer service team.

One of the problems with testing is simply that: you can't predict what new and
innovative way people will come up with to mess with your code. Humans are just
to good at thinking there way around barriers with weird and unexpected methods
(consider Hannibal and the Romans). But this is a problem when we try to test
our code because what we test against is overwhelmingly written by the same
developers who wrote the code their testing. For instance, when I created the
bug concerning emails I was using tests against a whole slew of test emails - it
simply didn't occur to me that there could be a plus in the name of the
mailbox. This leads me to my first point concerning testing,

## Whenever Possible - Don't Write Your Own Tests!

If at all possible try to get someone else to write some tests for your
code. Not only is this a great way to have some collaboration on a project, but
adding another person into the equation will almost certainly increase the
variety of cases your dealing with. More than that, since your colleague hasn't
seen the code you wrote they are less likely to bias the test cases against what
has already been considered.

Actually this can be a fun type of competition, with one person trying to stump
your program with weird edge cases and you trying to write foolproof code. This
is, after all, an evolutionary process and your wont evolve if it doesn't
encounter harsh competition, at least not with any speed.


### Use Unknown Data

The problem and solution I've outlined is hardly news to anyone in the testing
world, and developers have been working on solutions long before I developed my
half baked theories. One of those solutions is to run the tests with random
data. This can be either locally randomized or pulled from the web. The massive
data sets of leaked passwords/emails/usernames/etc... are a great opportunity to
throw some real world entries at your code and see what happens. Although I
stand firm in the belief that nothing will beat another person working to create
interesting tests, them using insightful data is a sure way to make their case
more rigorous. Moreover, in the case when you are working on a project alone -
and sadly given modern tech culture that is all to often the case - using some
outside data can help maintain an impartiality to tests when the randomness of
another human is hard to come by.

## Test Driven Development

In order to get some unbiased cases in your tests its entirely seemly to write
your tests before your code. Indeed this kind of test driven development is
usually always better than the alternative. On a large scale its a necessity,
after all the first thing you do when starting a project is outline what it
should do and maybe a few thoughts on how. Test driven development expands this,
effectively turning your project into a series of functions your have to
implement.

There are a myriad of articles on how and why test driven development is
important but it does have one drawback: it's slow to prototype. It's a truth
universally acknowledged but rarely spoken that almost the only time programmers
stick anywhere close to best practices is while writing blog posts about them
(myself included). No matter how strict we are, there's always going to be a
case where we need to do something quickly and we can't afford the hours/days it
would take to plan out the project ahead of time. Compounding the issue, the
times when time is of the essence can correspond to the development of some of
the most important aspects of a piece of software - Mr. Murphy has little
sympathy for the aspiring developer.

So, how can we overcome these types of issues? Well the sad answer is
that there isn't a silver bullet out there. You can work with strongly
typed language like Haskell, attempt to document weak points to return
to or, as is often the only way - entirely refactor the project at a
later date. On the subject of testing I can only make the following
observation, make each test count - that is:

## Only Test What Needs Testing

Yes, I hear your cries of "coverage" and "quality control!" However, this is a
real problem. As a junior developer, it's something I have often struggled
with. The process goes something like this, you'll go to look at a testing
framework, which has a simple function like:

```haskell
makeUrlFriendly :: String -> String
makeUrlFriendly = fixFalsehoods . replace " " "-" . toLowerCase
  where
    fixFalsehoods = replace "boring-wozniak" "steve-wozniak-is-not-boring"
```

And then a series of clever tests

```haskell
assert $ makeUrlFriendly "XKCD" == "xkcd"
assert $ makeUrlFriendly "E Plurabus Unum" == "e-plurabus-unumm"
assert $ makeUrlFriendly "Boring Wozniak"  == "steve-wozniak-is-not-boring"
```

Obviously most test frameworks have more user friendly options than `assert` but
I'll leave you to read about them on your own time. What I want to get at is
that these tests accomplish a sum total of nothing - they simply restate what
the function clearly does with a few needless examples. Of course for the
purpose of explaining the testing syntax thats fine, its often easier to work
with dummy functions than actually complex code. The problem comes from people
interpreting these examples as instances of what needs to be tested.

When I first started writing tests I spent hours writing needless tests like the
above for a whole host of code that I already knew worked. There is something
addictive about getting a nice series of checkmarks when you run a test. Yet by
the time I got around to testing the code that actually needed it I ran into
trouble. In order to fix the errors that my tests pointed out, I had to modify
the various supporting functions which I'd tested earlier - this in turn broke
their tests, since they basically asserted that the function worked in the way
it was written rather than that it worked correctly.

The problem is, of course, obvious. I was testing functions that had no business
being tested and as a consequence, rather than improving the quality of my code,
I had boxed it in with useless restrictions on helper functions. Testing only
for the sake of testing, gets you less than nothing, as I found out. It is only
by testing what we know to be shaky that we improve our code. This is also a
constant hurdle in test driven development, because it can be tempting to
needlessly set up tests for helper functions which have no edge cases, thereby
ensuring that these helper functions can not evolve during the development of
more complex components.

As a rule of thumb I present the following, incomplete and non universal list of
does and do not's when writing tests:

#### If you know the outcome of the test, don't write it.

This is exactly the case of our `makeUrlFriendly` function. It is entirely clear
exactly how the function will behave, there are no strange edge cases we might
miss in a quick glance at the code, and hence there's no reason not to the omit
the test.


#### Test to preserve legacy access.

If you are writing an API or any type of publicly accessible framework, it's
often important to support legacy methods (or risk an angry hoard of developers
throwing shade on GitHub). Tests are possibly the easiest way to do this, since
if you've tested the old version of your project thoroughly it remains only to
leave those tests in place to make sure new features aren't breaking old
workflows.

#### Test when you don't know what the outcome will be.

Suppose we rewrote our `makeuUrlFriendly` method with some added functionality,
perhaps dealing with special characters, even swear words - to the point where
it was no longer clear how it would garble input. This is an excellent time to
test since we can't see clearly how our method is working, so the test not only
verifies the correct approach but acts as a form of documentation in providing
understandable examples.

#### Tests are less important than writing clear code.

This last point is a bit more controversial, but nonetheless important to touch
upon. It is my belief that if you are faced with a choice between writing clear
code and testing it thoroughly, clean code should come first.

There are a few reasons for this,

- Well written code is less likely to have a mistake a test would reveal.
- Well written code is easier to maintain.
- If you can't afford the time to write good code, any test you write won't be
  worth the magnets its stored on.

The downside of course being the potential that you might overlook some key flaw
in your code that is then deployed to production - however writing something you
have confidence in rather than testing something you don't know will work seems
a well reasoned call.

Really the choice here comes down to philosophy, is it better to iterate over
something quickly, or make something of surpassing quality. It is my belief that
you can't beat quality, which is why I prefer handmade furniture to mass
produced pieces - but each has its own place depending on the circumstance.

## In Short

Testing is an integral part of development, but it is only a part of
development. Don't become so consumed with making sure testing work that you
neglect to write good code or that you start testing obvious statements instead
of what needs to be tested. Testing should never feel like a constraint to
development, but rather as a welcome addition - one that you would not *want* to
be without. If you find yourself testing only for the sake of checking off some
requirement it might be a good time to re-evaluate and make sure your not just
writing tautologies but actually discovering something about your code.
