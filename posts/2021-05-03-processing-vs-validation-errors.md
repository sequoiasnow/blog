---
title: Processing vs. Validating Errors
tags: programming, errors
---

Let's talk about how and when we generate errors. It is entirely common for the validation of incoming data to succeed, only for the later use of that data to be meet with some unforeseen difficulty later on in the program. However, since the former step is usually the only one to report errors, we are often left with inscrutable 500 responses in the second case.

Now, this has two very obvious consequences: the error is made unclear to the user, and the error is logged incorrectly for debugging. The first case can be the most severe - for instance, a user facing a banking form should never be left with a 500 response, the error must **always** be explicit in situations where trust in the programs logic are paramount. The other angle is more subtle, but amounts to an inconsistency in program design. Data that is for some reason invalid is a client error, and should be returned as such to a client to be handled. If an error that should fall under the auspices of validation is instead reported using logging, or crash reports, on the server, it finds itself misrepresented, and what is more - prevents the client from any kind of reasonable error handling.

Let's look at an example in practice. Suppose our application is concerned with storing and presenting memos. We have a function which validates an incoming memo:

```haskell
validateMemo :: Memo -> m (Either MemoErr Memo)
```

Perhaps this checks if a memo with the same name already exists in the database, or performed some other checks for nonzero fields. We then either return the error or the validated field, let's look at this in the context of a [Servant](https://hackage.haskell.org/package/servant)-like handler.


```haskell
postMemo :: Memo -> m (Entity Memo)
postMemo memo = do
    res <- validateMemo
    case res of
        Left err -> throwError $ error400 { errBody = err }
        Right memo -> runDb $ insertEntity memo
```

> The Haskeller who spends to much of their time browsing through hoogle, might also spot the slightly more obvious
> ```either (\ e -> throwError $ error400 {errBody = e}) (runDb . inserEntity) . validateMemo```
> option, but to each their own.

Now, let's look at the flow our potential client would expect: A memo is posted, either we receive a `400` and attached `MemoErr` or a `200` response and an `Entity Memo`, indicating successful database insertion. However, what we might not expect is a blank `500` response, after all it doesn't seem to be present in any of the above code. Not so fast however, let's think about what it means to have a function like `runDb` in our code - it means we are accessing the outside world (You might also see this in the relevant type classes for `m` had I not been to lazy to write them). When we are accessing the outer world, we are running the risk of errors, and hence it is apparent, but not obvious, that the insertion step of the above code can in fact result in an error!

Now, if that error is something unavoidable, i.e. the database is down, 500 is clearly an acceptable response. The problem here is when it's not. Suppose our insertion statement fails because our memo violates a uniqueness constraint? In theory that would be checked by our validation request, but since the two do not take place within a transaction, there is no guarantee that a memo might be created between the two steps with an overlapping name. In that event, we hit a 500 error from an insertion that should have been returned as a validation issue! In short, we hide a validation error that emerges as we process the request.

---

Now that entire example might seem a bit contrived, and it is for the purposes of demonstration, but this type of problem is ubiquitous in even the best code that touches the outside world. As soon as one point of an entry's validation becomes dependent on an unreliable external resource, we can not guarantee that a validation of a resource remains valid.
