# An Example Mycroft Application



The following is an example of a theoretical Mycroft application. The application itself is quite simple, it's a simple searchable list of HTTP status codes. As a static site, that "searchable" is accomplished in a rather unique way, by pre-rendering all of the possible status codes, since this is bound to result in less than $5 * 10 * 10 = 500$ possible pages, this is not to great a burden, and is an interesting problem regardless.

Let's start with the data, for each status code entry we have a **name**, a brief **summary** and a longer **description**. We want the summary to be less than 30 words, but allow for markdown or similar styling. Likewise we wish the description to be written as markdown or something similar, and the name should be plain text. Let's look at how we would define this data in our Mycroft application.

First, let's create a record which contains the accompanying metadata about our status code.



```haskell
import Mycroft

data HTTPCodeMeta = HTTPCodeMeta
  { codeName :: Text
  , codeDescription :: Content
  , codeSummary :: LengthRestrictedContent (Words 30)
  }
```



Here we use one of the useful types provided by Mycroft, `LengthRestrictedContent` is simply a `newtype` wrapper around Content with a phantom type parameter indicating the maximum length of the content. If we look at it's definition we can see that the reader instance for `LengthRestrictedContent` uses the underlying `Content` reader along with a simple length check of the source material as well.

```haskell
module Mycroft.Types.LengthRestrictedContent
  ( LengthRestrictedContent
  , pattern LengthRestrictedContent )
where

import Mycroft

newtype LengthRestrictedContent n =
	UnsafeLengthRestrictedContent Content
	deriving (Show, Eq, Read)

pattern LengthRestrictedContent <- UnsafeLengthRestrictedContent

instance (AsCharacter n) => MycroftReader (LengthRestrictedContent n) where
  readFromRaw t
    | length t < (chracterVal $ Proxy @n) = Left $ "This field should be no longer than " <> show (lengtVal $ Proxy @n)
    | otherwise = UnsafeLengthRestrictedContent <$> readFromRaw t
```



We can then create a custom type to encompass both the status code and it's metadata,



```haske
data HTTPCode = HTTPCode Natural HTTPCodeMetadata
```



It remains now only to specify how we wish to read that data into our application? Suppose we wish to define a reader from JSON/YAML, we do this by simply specifying an instance of the relevant class.



```haskell
instance FromJSON HTTPCode
  where parseJSON = withObject "HTTPCode" $ \ o ->
    do code <- o .: "code"
       name <- o .: "name"
       descripton <- o .: "description"
       summary <- o .: "summary"
```

Then in our application let's get the relevant data.

```haskell
app :: Mycroft ()
main = mycroft def $ do
  httpCodes <- readFile "codes.json" @[HTTPCode]
```

Note that we use the simple `readFile` command which will infer the relevant reader from file ending and available conversions. We could also be more explicit and write

```
readFileWithReader "codes.json" jsonReader @[HTTPCodes]
```

> The `@[HTTPCodes]` is a succinct way to indicate what the file should be parsed as, since the type checker can not infer which type of `FromJSON` to use.

To get the short of it, we have now specified a document we wish to read from, we can even generate a basic template of it in our application by passing the `gen-empty-files` option, which yields something like:

```json
[
    {
        code: 1,
        title: "",
        name: "",
        description: ""
    }
]
```

Our codes in hand we can now get down to business.



## Part II - Building Our Pages



Let's start by discussing the basic layout of our application. It consists of a large main page with each of the status codes. There are, pre-rendered, all the pages of searches which can be performed, each of these lists a subset of all codes given the search term then there are pages with each code, described in detail. Let's start with the last, first we'll create a function to render a single code, and optionally expand it.

> In this example we'll be using Lucid, but there are of course other HTML library's available.

```haskell
renderCode :: HTTPCode -> Bool -> Mycroft (Html ())
renderCode HTTPCode code (HTTPCodeMeta name sum desc) expanded = do
  let wrapTitle = if expanded then return else \ x ->
    -- using mycroftLink we require that the page we are linking to exists
    -- locally, and we will be warned if we fail to create it later.
    do link <- mycroftLink $ root <//> code
       return $ a_ [href link] x

  let wrapDescription = if expanded id else (const "")
  return $ article_ $
    do wrapTitle $ h2_ $ code <> "-" name
       section_ [class_summary] $ contentToLucid summary
       section_ [class_ description] $ wrapDescription $ contentToLucid description
```

There are perhaps clearer ways to write the above, but of course that is the prerogative of the person developing the application. However, the important thing is we now have a function which can create the html for our status code, and even create a link for it as needed.

Now, let's return to our main page, and create the pages to be linked to by each of our codes.

```haskell
forM_ codes $ \ code -> createPage HTML (root <//> code) $ lucidToMycroft $
    html_
```
