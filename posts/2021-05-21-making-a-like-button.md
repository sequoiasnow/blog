---
title: Building a Like Button
tags: programming
---

Let's look at a very simple exercise: adding a like button to a site, such as this one. How would we go about it? It's not as easy a question to answer as we might assume. Here's the thinking:

Frist, we need to associate our blog post with it's number of likes, say we have some type `Post`, we need define something like `type Post' = (Post, Int)` which associates the number of likes with our post. Then we can add a button somewhere on our site, to submit a `like` request. This might look like

```html
<form method="post">
    <button class="like">
        <span class="icon">&#9829;</span>
        <span class="icon-sub">{{likes}}</span>
    </button>
</form>
```

Here we display a heart icon with the number of likes and the entirety of the thing is a form. Our handler can then work to update our entry in the database. (In this example let's use the small web framework [Spock](spock.li)).

```haskell
server :: Spock
server = do
    getPost ("post" <//> var) $ \ postName ->
        -- assuming we have some database method to get a post
        post <- runDb $ DB.getBy $ UniquePostName postName

        -- and using some generic function to render a post as html
        renderPostPage post

    post ("post" <//> var) $ \ postName ->
        -- pseudode for database update
        newPost <- runDb $ DB.update
          (\ p -> p ^. PostName  ==. val postName)
          (\ p -> p { postLikes = 1 + postLikes p })

        -- return rendered page
        renderPostPage newPost
```

> The above code uses a form for ease of use, but it's easy to see how the exact same code would be appropriate for use with a JSON api.

It's a pretty simple setup, although we've fudged some of the code's details. Still the idea is simple, and we pat ourselves on the back and call it a job well done. Well... not quite. There are a few pretty obvious failures, the most prominent one being, there's no way to ensure the same person won't spam that like button. It'd be easy enough for some nefarious actor, or disgruntled post author, to spam that like button. What can we do to prevent this?

Well the first idea is simply to limit liking to logged in users, if it's a site that permits logged in users that is. But that second point hits on a problem, there are a lot of sites which serve up public content without any facility for logging in, like this one. Moreover, even when such an administration system does exist it doesn't mean you'd want to restrict the ability to comment to that.

So we have to come up with a way to make sure someone reading an article can only like it once, and before going any further let's bring the ax down on one concept: fingerprinting. In general it's not a particularly ethical or democratic thing to track someone without their permission. Furthermore, computers doing things without explicit user interaction can often lead to more confusing than leaving a few steps to a human to make explicit what's going on. Our solution then, shouldn't rely on weird underground trickery in attempting to pin our users down, instead we should think about what we can ask them to limit their multiple likes.
