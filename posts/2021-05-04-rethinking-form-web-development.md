---
title: Rethinking Forms For Web Development
tags: form, haskell
---

## Web Applications

Let's take a little walk back into the day's of 90s web development and talk
about forms. Forms are those ubiquitous collections of fields which you fill out
to sign up for a service, log in, or otherwise perform a function. They are the
web's analog to the physical forms you might send to order a book, or file your
taxes.

Now, that's a pretty familiar definition - we are all aware of forms, and the
role they play in our daily web development. However, their role in recent years
has taken somewhat of a back seat to the emergence of web applications. In a web
application, forms are typically restricted to the role of signing up and
logging in, and even then their submission is often subsumed by javascript,
rather than typical server validation. The core of the application is likewise
driven by javascript with only data persistence being relegated to the
server. In this manner the progressive web app mirrors the structure of the
native applicaiton, though it still uses the frameworks of html and css to
render the content.

Taking this approach is not without some downsides however. For instance, since
the web has no equivalent of packaged standard libraries, using a web app means
downloading a massive bundle of basic resources required to run the actual
application code. Fortunately their are mechanisms in place to cache these large
resources, but that caching is largely useful only when a web application is
regularly visited. Less commonly frequented sites are hamstrung by large loading
times. Moreover, the introduction of so much bloat in the development of a non
native framework means that, even as JavaScript interpreters become increasingly
fast, the increase in code bloat means the perceived speed of a web page is
often slower now than it was years ago.

Now, none of these factors are themselves a reason to avoid the use of
JavaScript based web applications - their are many situations where they are
quite simply the most efficient and elegant solution to a problem, social media
for instance. But it does merit some consideration of *when* to reach for
them. In the context of a site which does not handle multiple asynchronous
inputs and constant user input, as social media websites do, it may very well be
that a more traditional approach may do. Thus, let's take a look at that older
way...

## The Early Web

Before the past decade when javascript took the world by storm, web development
was a pretty regular concern. There was a server, which rendered a page of
html. When a link was clicked or a form submitted the server would respond by
sending back a new page of html. From this rather complicated applications were
constructed, relying almost entirely on the submission of forms in order to
collect the users input. One of the benefits of this approach was that server
logic resided on, you guessed it, the server - which meant that their was no
need to download complex frameworks to the client beforehand, they merely
received some small `KB`s of website data that was quickly rendered to the
client. As a result of this simplicity, and the incredible increase of
processing power and network speed over the past decades, an website built on
these older principles seems incredibly fast compared with the hulking monoliths
of javascript served up today.

There are other benefits as well. Because forms were so ubiquitously used in
early web development, many fundamental aspects of web development exist to
ensure they are more user friendly and workable. Moreover, us users have become
acclimated to the web / submission workflow, to the point where the client side
validation provided by many javascript based apps actually seems
unintuitive.

> Classic examples include a field complaining that what you've entered isn't an
> email after typing the first character, or a form which passes all client side
> checks being submitted only to be rejected because of some unforeseen error
> which is not made visible.

The downsides, of course, are as you would expect. It is more difficult to build
any type of *application* logic using only server side renderings, just as it is
difficult to build a form based logic using JavaScript. However, in between
there are a wide variety of uses for server based websites: blogs, document
stores, event sign-up's, money transfers, etc.. In such a project using a server
side approach not only improves the projects performance, but can also induce
confidence in an applications by adhering to a behavior pattern familiar to
websites - banking applications are an excellent example.

## In Development

Their are other benefits to serving up a server based webiste: there is no need
to create a version-ed API as the html is by definition synced with its
server, consequently their is no potential block between backend and frontend
development. However, most importantly, the user interacts with an application
in a way that is innately obvious, since it is the behavior the web was built to
support. Of course, the added performance benefit doesn't hurt...


> For more implementation details see the next post...
