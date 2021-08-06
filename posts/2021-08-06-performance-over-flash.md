---
title: Performance Over Flash
tags: programing, web, performance
---

A consistent theme of this blog, aside from the irregular and convoluted nature of the posts, is a focus on simplicity. The notion of functionally driven development has become ever-more appealing as over the past few years [websites have seemingly gotten slower](https://pxlnv.com/blog/bullshit-web/). Its frustrating when you realize the extra 10 seconds it took your banking app to load were not increasing your experience, but rather loading a litany of libraries to track your activity, suggest you adds, and create animations which do nothing more than delay you actually reading the statement you were looking for. However, though the phenomena of a bloated web is something I've talked about before, it has largely been in the form of a rant on what to take away, things that negatively impact a users experience: 

- animations (fun once, annoying twice, aggravating thrice,...)
- tracking 
- targeted adds
- auto playing video 
- loading the entire javascript applet to display one page
- etc...

Yet the conversation thus far has been largely negative, without mention of constructive solutions, beyond stripping away the bloat. One of the things which we miss in the conversation is the possibility for *subtle* design. Although it seems to me that page bloat, and the associated inability to perform the task you visited a site to do, is the most aggravating part of the web today - the alternative, a bare, unstyled stack of forms, is not particularly appealing either. So, what aspects characterize the balance? An application or site which puts function first whilst retaining a unique style that embellishes rather than detracts. 



Well we have already hit upon some of the negatives, and there are more. Robert Heinlein's Words concerning government come clearly to mind, 

> In writing your constitution let me invite attention to the wonderful virtue of the negative! Accentuate the negative! Let your document be studded with things the government is forever forbidden to do... What I fear most are affirmative actions of sober and well-intentioned men, granting to government powers to do something that appears to need doing.
>
>    -- Prof. Bernardo de la Paz, *The Moon is a Hash Mistress*

These libertarian idea have a certain resonance in the context of user experience. There are certain qualities which a user will not enjoy no matter what, or go against the ideals of the people creating a website (such as tracking) which should be avoided no matter what. To often it becomes plausible later in a project to add a "feature" intended to solve some problem or add some functionality which contradicts one of these principles, and the price of that convenience falls to the user. 

Any positives that follow a declaration of things to avoid, should follow the principle of the negative, and be as minimal as possible. To paint with the thinest brush possible, and in the most gentle way possible. Consider the declaration of a font, typography is an essential part of web design, but let us not let that fact alone declare our use of such fonts. Instead, add typography minimally and only where important, is it better to increase load times for the sake of customization or have we already declared that style should not impact speed? If we are to use custom fonts, we should approach it minimally, using only one where needs the most impact and slowly building from there. A similar process exists with the inclusion of JavaScript libraries, or images, or animations, etc... In every case the idea is to selectively build up from nothing rather than start with a framework and trim it down. By expressly started with the negative we assert the principles of our design and let it grow organically within those limits. 

In the same way that [CSS Zen Garden](http://www.csszengarden.com/) which years ago proved that with good markup, almost any design was possible in CSS, so too is it possible to build a beautiful yet functional design by first explicitly defining what can not interfere with functionality, and then building wildly within those constraints.

