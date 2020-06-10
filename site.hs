--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Hakyll
import           Text.Blaze.Html                 (toHtml, toValue, (!))
import           Data.List              (sortBy,isSuffixOf)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           System.FilePath.Posix  (takeBaseName,takeDirectory,(</>))
import qualified Data.Set as S
import           Text.Pandoc.Options

--------------------------------------------------------------------------------

root :: String
root = "https://thoughtsfactorial.com"

--------------------------------------------------------------------------------

-- A list of tags to be rendered as icons
tagIcons :: [(String, String)]
tagIcons =
  [ ("science", "beaker")
  , ("flame", "flame")
  , ("photography", "device-camera")
  , ("education", "mortar-board") ]


-- Render a singular tag as html
renderTag :: String -> Maybe FilePath -> Maybe H.Html
renderTag tag mbPath = Just $ case mbPath of
  Just filePath ->
    H.a ! A.title  (H.stringValue $ "All pages tagged '"++tag++"'.")
        ! A.href   (toValue $ toUrl filePath)
        ! A.class_ (H.stringValue $ "tag tag--"++tag++iconClass)
        $ tag'
  Nothing ->
    H.span ! A.title  (H.stringValue $ "All pages tagged '"++tag++"'.")
           ! A.class_ (H.stringValue $ "tag tag--"++tag++iconClass)
           $ tag'

  where
    mbIcon    = lookup tag tagIcons
    iconClass = case mbIcon of
      Just _ -> " tag--icon"
      _      -> ""
    tag'      = case mbIcon of
      Just icon ->
        H.img ! A.src (H.stringValue $ "/images/icons/"++icon++".svg")
      _ ->
        toHtml tag


--------------------------------------------------------------------------------

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p
                                 </> takeBaseName p
                                 </> "index.html"
                           where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
    where
        idx = "index.html"
        clean url
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  -- Compile post tags for the site
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  categories <- buildCategories "posts/*" (fromCapture "cat/*.html")

 -- Add more information to template context
  let postCtx = tagsFieldWith getTags renderTag mconcat "tags" tags     <>
                tagsField "nav_categories" categories                   <>
                dateField "date" "%B %e, %Y"                            <>
                teaserField "teaser" "content"                          <>
                constField "root" root                                  <>
                defaultContext

  -- Compiles a tags page, in homage to the archive page.
  tagsRules tags $ \tag pat -> do
    let title      = tag
        titleHtml = maybe tag renderHtml (renderTag tag Nothing)

    route cleanRoute
    compile $ do
      posts <- recentFirst =<< loadAll pat
      let ctx = constField "title" title                 <>
                constField "title_html" titleHtml        <>
                listField "posts" postCtx (return posts) <>
                defaultContext

      makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
          >>= cleanIndexUrls


  -- Compiles Categories

  -- Copy images over
  match "images/**/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  -- Compile Clay CSS rules
  match "css/*.hs" $ do
    route   $ setExtension "css"
    compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

 -- Copy an static CSS assets that might come up
  match "css/*.css" $ do
    route idRoute
    compile copyFileCompiler

  -- Copy an static JS assets that might come up
  match "js/*.js" $ do
    route idRoute
    compile copyFileCompiler

  -- Create an about/contact page
  match (fromList ["about.md", "contact.markdown"]) $ do
    route   $ cleanRoute
    compile $ pandocMathCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
        >>= cleanIndexUrls

  -- Create the 404 page, notably don't create a cleandRoute for it
  match "404.md" $ do
    route   $ setExtension "html"
    compile $ pandocMathCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- Copy posts
  match "posts/*" $ do
    route cleanRoute
    compile $ pandocMathCompiler
        >>= saveSnapshot "content" -- allows us to pull teasers
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  -- Create the Tags page
  create ["tags.html"] $ do
    route idRoute
    compile $ do
      posts <- loadAll "posts/*"

      let tagsCtx = constField "title" "Tags"                <>
                    listField "posts" postCtx (return posts) <>
                    tagCloudField "tagCloud" 50 150 tags     <>
                    defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/tags.html" tagsCtx
        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

  -- Create a sitemap
  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      singlePages <- loadAll (fromList ["about.md", "contact.markdown"])
      let pages = posts <> singlePages
          sitemapCtx =
              constField "root" root                    <>
              listField "pages" postCtx (return pages)
      makeItem ""
          >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
              listField "posts" postCtx (return posts) <>
              constField "title" "Home"                <>
              defaultContext

      getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

  match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
  let
    mathExtensions =
      [ Ext_tex_math_dollars
      , Ext_tex_math_double_backslash
      , Ext_latex_macros
      ]
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    newExtensions = foldr enableExtension defaultExtensions mathExtensions
    writerOptions =
      defaultHakyllWriterOptions
      { writerExtensions = newExtensions
      , writerHTMLMathMethod = MathJax ""
      }
  in pandocCompilerWith defaultHakyllReaderOptions writerOptions
