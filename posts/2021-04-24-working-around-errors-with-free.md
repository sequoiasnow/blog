---
title: Working Around Errors With Free
tags: haskell, programming
---

In the course of writing most any program your liable to run into two main
categories of errors. Those caused by forces within your control, and those
caused by forces without.

## The Unavoidable

These are things outside of your control, dropped database connections,
improperly configured configuration files, etc... Each of these is by nature a
permanent failure since there is no effective way for the application to recover
the lost functionality. As such these are the errors that tend to cause a
program to fail, and now handling can prevent.

## The Recoverable

Then there are the logical errors which are introduced by edge-cases in your own
programs logic. For instance, the accidental deletion of a user during an active
session, causing an expected resource to be unavailable. While these errors
could be solved by simply bubbling up an error, they are also in some manner
avoidable - they must be, since they are in some manner caused by the programs
logic.

---

Recoverable errors are much more favorable than unavoidable errors for the
simple reason that they allow us to extend functionality rather than limit
it. For instance, consider the following (rather contrived) example of a user
attempting to like a message that has been deleted by another user. We could
simply throw an exception,

```haskell
likeMessage :: (MonadThrow m, Queryable m) => MessageId -> m Message
likeMessage mId = either throwError id $ runDb ...
```

but this limits our user's information about what has happened. Instead, we can
add in functionality through actively handling our error. Not only does this
improve our user's experience, but also informs the type of our function. It's
potential failure is now entirely obvious:

```haskell
data ModifyMessageErr = ErrMessageDeleted

likeMessage :: (Queryable m) => MessageId -> m (Either ModifyMessageErr Message)
likeMessage mId = either (const ErrMessageDeleted) id $ runDb ...
```

Moreover, if other potential failure points exist we can extend the
`ModifyMessageErr` type as appropriate. Seemingly we have eliminated the
potential for an unexpected halt introduced by `MonadThrow`, but have we?

> Our current recoverable error handling has some issues as well, namely it
> simply bubbles up the responsibility using Either instead of
> `throwError`. Fortunately both of these problems have a similar solution.

See although we have no introduced some, incredibly basic, handling for a
*recoverable* error, the use of `runDb` is inherently `IO` based - the database
is after all an external concern. Therefore, it too has the potential to fail,
and hence it's easy to see that `MonadIO m => Queryable m`. Ideally, we would
write our program, free of worry as to the condition of the database or our
message. But how do we do this?

## Pushing Back Responsibility

Consider what our function most basically needs to do, operate in some manner on
a message object. It doesn't actually need to do know the message `id` since it
is only working on the object itself. We can encapsulate that functionality and
pass it directly to the function, thus freeing `likeMessage` from any
responsibility other than the logic of incrementing the message.

```haskell
likeMessage :: Message -> Message
likeMessage m = m { messageLikes = messageLikes m + 1 }
```

However, `likeMessage` can no longer access the database, that responsibility,
is now pushed back. Eventually however, there must exist some function, to
perform the actual work...


```haskell
modifyMessage :: MessageId -> (Message -> Message) -> m Message
```

Now, on first glance, this function **must** use `IO`, and we find ourselves
again in the hole we dug in the first place with the `Queryable`
typeclass. However, we have a trick up our sleeve, [the `Free`
monad](/posts/2021-04-20-extending-functionality-with-free/). Suppose instead of
defining `modifyMessage` as a function, we define it as a command,

```haskell
data QueryCmd next =
    ModifyMessage MessageId (Message -> Message) (Maybe Message -> next)


instance Functor QueryCmd where
    fmap f (ModifyMessage mId modify getMsg) = ModifyMessage mId modify (f . getMessage)
```

We can now declare a `Free` monad which allows us to build up a chain of these
commands, without ever running them.

```haskell
type QueryM = Free QueryCmd
```

We can now define `modifyMessage` in the context of our `Free` Monad.

```haskell
modifyMessage :: MessageId -> (Message -> Message) -> QueryM (Maybe Message)
modifyMessage mId modify = liftF $ ModifyMessage mId modify id
```

The trick here, is the function `liftF` which as the function suggests, lifts our command to the Free monad.

```haskell
liftF :: (Functor f) => f a -> Free f a
```

The logic here is actually pretty simple, the `Free` monad acts as a wrapper which extends the functionality of a functor, `Free f a = Free (f (Free f r)) | Pure r`, and the `liftF` function simply places a functor `f a` into the chain,

```haskell
liftF f = Free (fmap Pure f)
```

Even if the innards of this transformation seem confusing the end result is quite simple to work with, we can transform a functor into a Monad, i.e. `modifyMessage`. We can then create an interpreter for `QueryM` that is independent of backend implementation. For instance, if we wanted some really basic testing we could use some predetermined values.

```haskell
run :: QueryM a -> State [(MessageId, Message)] a
run (Free (ModifyMessage mId updateMessage getMessage)) = do
    message <- gets (lookup mId)

    -- If we found the message modify it
    forM_ message $ \ m -> modify (insert mId $ updateMessage m)

    -- run next
    run (getMessage message)
```

Now, let's look at what's going on here. In this case we are transforming our
`QueryM` monad (which is effectively a chain of `QueryCmd`s) into a `State`
monad. The logic of our program is now entirely pure, and we can easily create
another transformer to `IO` using `runDb`, but crucially not part of the
application logic is at risk from *any unavoidable errors* since by construction
all sideaffectful code is contained within the `run` function. Not only is the
`run` code now explicitly responsible for all external errors, but it can also
determine the manner in which we wish to handle internal conflicts with that
external conflict. For instance our current definition of `ModifyMessage`
includes a `Maybe Message` indicating that the given message might not exist to
be modified. However, had we defined it as simply `Message` the responsibility
of finding such a message, or throwing and error would be pushed to the
interpreter of the `QueryM` monad. We might wish to always error in such an
event, create a new message, or whichever - but crucially the responsibility for
maintaining database consistency now lies with the backend we choose to run our
program rather than the programs logic itself.

This is the real benefit of `Free` monads, or similar programs like
[monad-prompt](https://hackage.haskell.org/package/MonadPrompt), namely by the
choice of commands we effectively separate concerns between the logic of the
application and the resolving of inconsistencies with external resources, files,
databases, etc.. By making our program execution agnostic we gain fine grained
control over exactly which errors should be acceptable and handleable and which
are unavoidable and should be the responsibility of the relevant runner.
