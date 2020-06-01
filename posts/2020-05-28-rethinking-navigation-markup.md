---
title: Rethinking Navigation Markup
tags: programming, web, html
---

The navigation for this blog is reasonably simple - partly because I've always
found unnecessarily complex navigation to be tedious, and partly because
minimalism is a theme in the design. We have three categories:

- A title segment linking to the home page.
- Several links to static pages.
- Some links to popular tags.

At the time of this post, it looks something like:

![Navigation Tree](/images/2020-05-28/nav.png){width=168px}

Now, in my initial attempt I wrote this as follow:

```html
<header class="nav">
  <div class="nav__wrapper">
    <div class="nav__title">
      <a href="/">Thoughts<span class="factorial">!</span></a>
    </div>
    <nav class="nav__pages">
      <a href="/">Home</a>
      <a href="/about/">About</a>
      <a href="/contact/">Contact</a>
    </nav>
    <nav class="nav__tags">
      <a href="/tags/math/">Math </a>
      <a href="/tags/philosophy/">Philosophy</a>
      <a href="/tags/programming/">Programming</a>
    </nav>
  </div>
</header>
```

There are some clear problems with this that I failed to notice while iterating
over designs. Sometimes it's easy to become so focused on rapidly prototyping
new styles that common quality control gets left behind. Still, it's never to
late to return and fix up some glaring errors. Let's look at some of them.

## Using `nav` Correctly

According to [w3schools](https://www.w3schools.com/TAgs/tag_nav.asp),

> The `<nav>` tag defines a set of navigation links.
>
> Notice that NOT all links of a document should be inside a `<nav>`
> element. The `<nav>` element is intended only for major block of navigation
> links.
>
> Browsers, such as screen readers for disabled users, can use this element to
> determine whether to omit the initial rendering of this content.

Although we can use multiple `<nav>` tags across the site, it seems these are
best understood as each applying to a separate navigational area. Since the
chunk of html we are working with describes a *single* navigation tree, it makes
since for the html to reflect that. We can make that change easily enough by
wrapping our links in a list:

```html
<nav>
  <ul>
    <li class="nav__title">
      <a href="/">Thoughts<span class="factorial">!</span></a>
    </li>
    <li class="nav__pages">
      <ul>
        <li><a href="/">Home</a></li>
        <li><a href="/about/">About</a></li>
        <li><a href="/contact/">Contact</a></li>
      </ul>
    </li>
    <li class="nav__tags">
      <ul>
        <li><a href="/tags/math/">Math </a></li>
        <li><a href="/tags/philosophy/">Philosophy</a></li>
        <li><a href="/tags/programming/">Programming</a></li>
      </ul>
    </li>
  </ul>
</nav>
```

Note that the wrapper div around the title link is no longer an anomaly, but
fits in properly with the semantics of the remainder of the list.

Moreover, though the above code is actually more nested than the original, its
now very clear what's going on. In fact we could add titles to our sections if
they every grew to large, or add new ones as need be without having to write
additional css. The class names remain only to differentiate borders and text
size and could be made superfluous if we changed from the BEM naming convention.

## Superfluous Wrappers

Really there's no need for the `<div class="nav__wrapper"> ... </div>` element,
since our navigation makes absolutely no use of it. Granted, there are times
when wrapping elements are a necessity, but these cases are astoundingly rare,
usually being equivalent to some margin / grid specification. So, away with the
unneeded `div`. The same goes for the `header` element, which itself serves only
to confuse the situation as we have not the placed the `nav` element in an
`article`. Hence we are left only with the abbreviated code above.

## Conclusion

Although the changes above are relatively simple, it can be very easy to ignore
the small flaws in a working project once a prototype is done. I certainly have
done this on a multitude of occasions, and I can say with certainty that fixing
a problem when its spotted rather than assigning it to a todo list always leads
to a better maintained project and saves a massive headache in the future.

Keeping that in mind, what have we accomplished?

- The navigation element has eliminated superfluous wrappers which could confuse
  future styling
- The navigation has been made more accessible due to a correct use of lists and
  the `nav` element.
- Our styling can be easily generalized to any additional segments we might
  decide to add in the future.
