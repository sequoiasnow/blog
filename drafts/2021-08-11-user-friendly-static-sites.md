---
title: User Friendly Static Sites
tags: thoughts, plans, action
---

Let's talk about a problem which can plague the development of a small static site: editing content. The problem is as simple as it is common, how to deal with a non techsavvy user who needs to edit the finished product. This problem can extend to developers as well, many of whom would prefer to write in markdown than remember the explicit keycodes for special html characters. Now, there are solutions, but it's interesting just how many personal (and customized) sites are powered by Wordpress. A worthy platform surely, but comparatively expensive to host and rather bloated and large compared to most use cases.

There are other, less cumbersome, alternatives: namely static sites. Many cool and useful generators exist, Hakyll, Jekyll, next.js, Hugo... [the list goes on](https://github.com/myles/awesome-static-generators). Many are written in JavaScript, but search long enough and you'll find one for any language ([LambdaPad](https://github.com/gar1t/lambdapad) is written in Erlang for Christ's sake). However, one thing they have in common is that they generally appeal more to developers than to users. Even [11ty](https://www.11ty.dev/docs), one of the most popular static site frameworks requires data to be supplied as YAML or JSON.

Of course, for a small blog, where the only content that requires editing are blog posts, learning the small bit of YAML and markdown is not particularly cumbersome. Especially since markdown has become a mainstay of many applications outside the world of software development. However, there are many cases where that "little bit of knowledge" quickly spirals. Consider the wealth of custom properties on something as relatively simple as a personal site:

- links to social media
- photographs
- separate bio portions
- potential resume sections
- contact information
- ...

In a typical blog it can seem sensible to ignore these "meta" elements' complexity since the meat of the site lies in the much more comprehend able creation of posts. However, in a personal or informational site which is not post driven, these matters are suddenly cast into stark relief.

The problem, of course, is that a generic "small site" is a hard thing to quantify, since there are so many forms that content can take throughout. Wordpress tends to treat every aspect of the site as adaptations of posts, Drupal treats content as blobs, and other CMSs take similar approaches, generally grouping content as amorphous generic blobs or attempting to use a standardized form, like a post, in its place. Both of these approaches, or even combining them, have drawbacks in terms of usability. It's difficult to understand exactly how content blobs are rendered, since inherently they are filled with generic content.

That generalization can be a problem in two ways. Firstly, in the most restrictive case, the developer will render the content of a blob assuming it has a very particular format. That might be a title, of a certain length, a date string, an email address - the format of that component of the site is then fixed for the user. However, since the object rendered is inherently generic, it is possible for a user to render something that is not accounted for, and this too may cause problems. To overcome this, some systems allow typing for these informational, but generally only for generic types. When it comes to more specific instances this is much harder. Consider the example of a sites about page, we might have a list of employees stored somewhere, and then various positions, each of which should be filled by an employee. In most frameworks it's rather difficult to present this relationship, and even more so when we want to do it in a user friendly way.

The other side of the coin of course, is over-generalization, when a user may place anything within a certain context and have it rendered. The drawback of course is that a developer may only account for certain renderings, and again, indicating this to the user is particularly difficult. Using a generic form of data storage, like posts, has a similar problem, perhaps even more so, for it contains within it not one, but multiple types of content.

Overall, we are left without easy recourse for the creation of a user friendly custom site, no matter how simple. But, perhaps, there is a way.

---

Let's talk a little bit about [Pandoc](pandoc.org). Pandoc, is a tool for document conversion, it takes one format, converts it to an abstract syntax tree [AST] and from there into any format that may be rendered from that tree. Though Pandoc is used primarily for document conversion, it's strategy and parsing ability suggest an opportunity for the creation of a static site generator.

Consider the following hypothetical application. It allows one to define both readers and writers from an AST, but additionally to restrict which elements may be included in the relevant syntax tree depending on which component. If we define a website as a collection of components, each of these components could define acceptable abstract elements, and relevant rendering. On the other hand, custom readers could be implemented, thus allowing the reading of more complex types than are typically allowed under standard simply typed systems. The beauty of this approach is that the developer can work with abstract data and it's rendering, while the corresponding data given by the user can be arbitrarily generic and user friendly.
