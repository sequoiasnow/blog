-- "To Err Is Human"

{-# LANGUAGE OverloadedStrings #-}

import           Clay
import qualified Clay.Media as M
import qualified Clay.Filter as F
import           Clay.Stylesheet
import           Data.Monoid
import           Control.Monad (forM_)
import           Data.Text  (Text, pack)
import           Prelude hiding (div, span, rem, (**), not, all, filter)
import qualified Data.List as L



-- Render the stylesheet to be copied by Hakyll to the static site.

main :: IO ()
main = putCss blogStylesheet

--------------------------------------------------------------------------------

blogStylesheet :: Css
blogStylesheet =
  do

    -- Global reset styles to maintain some sense of predictability to the
    -- document.

    body ?
      do sym margin      nil
         sym padding     nil
         backgroundColor bgColor
         color           txtColor
         borderColor     txtColor
         textFont

    star  ?
      do boxSizing     borderBox
         borderColor   txtColor

    -- Overrides for dark mode

    "body.dark" ?
      do backgroundColor bgColorDark
         color           txtColorDark

    ".dark" ** star ?
      do borderColor     txtColorDark
    -- The Dark Mode Switch
    darkModeButtonStyle

    -- Common styles for standard html elements

    commonStyles


    -- General page layout, with three sections, a header, content and a
    -- footer.

    body ?
      do display        flex
         flexDirection  column
         alignItems     center
         justifyContent spaceBetween

    ".wrapper"  ?
      do query all [M.maxWidth (px 666)] oneColumn
         query all [M.minWidth (px 666)] twoColumn

         maxWidth   (px 960)
         minHeight  (vh 80)
         marginTop  (rem 1.7)

    -- Common styles for various areas spects of the site. In general these
    -- consist of a navigation bar, varying page content, and a footer.

    ".nav"         ? navStyle
    ".content"     ? contentStyle
    ".post"        ? postStyle
    ".postlist"    ? postlistStyle
    ".tag-cloud"   ? tagcloudStyle
    ".page-footer" ? pageFooterStyle

    -- Styles for elements common to various areas
    tagStyle

--------------------------------------------------------------------------------

-- Color Pallet

bgColor, txtColor, emphColor, tagColor :: Color

bgColor      = "#f9f7f4"
txtColor     = "#000"
emphColor    = "#807FE0"
tagColor     = emphColor

bgColorDark  = "#1D1D1D"
txtColorDark = "#FFFFFF"

offColors :: [Color]
offColors = L.reverse
  [ "#ffc94c"
  , "#fec056"
  , "#fcb85f"
  , "#faaf66"
  , "#faaf66"
  , "#f89e70"
  , "#f79574"
  , "#f58d78"
  , "#f4847b"
  , "#f4847b" ]

--------------------------------------------------------------------------------

-- Font styles

textFont, headerFont, codeFont, titleFont :: Css

textFont =
  do fontSize    (rem 0.9)
    --  fontFamily  ["Libre Baskerville", "Georgia", "Times New Roman"] [serif]
     fontFamily ["IBM Plex Mono", "Monaco", "Fira Mono", "Source Sans Serif"] [monospace]
     fontWeight  (weight 400)
     lineHeight  (unitless 1.618)

headerFont =
  do fontFamily ["IBM Plex Mono", "Monaco", "Fira Mono", "Source Sans Serif"] [monospace]
     fontWeight (weight 600)

titleFont =
  do headerFont
     fontSize   (em 2)

codeFont =
  do fontFamily ["IBM Plex Mono", "Monaco", "Fira Mono", "Source Sans Serif"] [monospace]

--------------------------------------------------------------------------------

-- Layout constraints for the site.

siteWidth, postWidth :: Size LengthUnit

siteWidth = px 960
postWidth = px 640


--------------------------------------------------------------------------------

-- Simple mixins to help eliminate unnecessary repetition

twoColumn, oneColumn, centerColumn :: Css

centerColumn =
  do display         flex
     flexDirection   column
     alignItems      center

oneColumn =
  do display       flex
     flexDirection column
     alignItems    flexStart

twoColumn =
  do display flex
     justifyContent center



--------------------------------------------------------------------------------

-- Additions to the framework for recent css properties.

transition' :: Text -> Time -> Css
transition' prop t = transition prop t ease (sec 0)

initialLetter :: Size a -> Css
initialLetter = prefixed (browsers <> "initial-letter")

--------------------------------------------------------------------------------

commonStyles :: Css
commonStyles =
  do
    a ?
      do textDecoration none
         color          tagColor

    time ?
      do codeFont
         fontSize      (rem 0.8)

    h1 <> h2 <> h3 <> h3 <> h4 ?
      headerFont

    h1 ?
      do fontSize  (rem 1.7)
         textAlign center

    h2 ?
      fontSize  (rem 1.5)

    h3 ?
      fontSize  (rem 1.2)

    hr ?
      do width           (pct 100)
         borderTop       solid (px 1) inherit
         borderBottom    solid nil black
         borderLeft      solid nil black
         borderRight     solid nil black
         sym2 margin     (em 1) nil


--------------------------------------------------------------------------------

navStyle :: Css
navStyle =
  do -- Set the general styling of the header.
     sym2 padding    0 (em 0.6)
     margin          0 (em 0.5) 0 0


     ".nav__wrapper" ?
       do sym2 padding    (px 5) (px 0)
          display         flex
          flexDirection   column

     nav |> a ?
       do sym2 margin    (px 5) 0
          transition     "all" (sec 0.2) ease (sec 0)
          textDecoration none
          color          inherit
          headerFont

          hover &
            color        emphColor

     ".nav__tags" <> ".nav__links" ?
       do marginTop     (em 0.5)
          display       flex
          flexDirection column

     ".nav__links" ?
       do borderBottomStyle dashed
          borderBottomWidth (px 1)

     ".nav__logo" ?
       do whiteSpace        nowrap

          a ?
            do fontSize       (em 1.2)
               borderBottomStyle solid
               borderBottomWidth (px 1)


     query all [M.maxWidth $ px 666] $
       do borderBottomStyle solid
          borderBottomWidth (px 1)
          marginBottom      (em 1)
          width             (pct 100)


--------------------------------------------------------------------------------

contentStyle :: Css
contentStyle =
  do -- Set common styles to the display of a post.
     textFont
     maxWidth        siteWidth
     width           (pct 100)
     sym2 padding    0 (em 0.6)

     --Style our code blocks with some line numbers and themes.
     pre ?
       sym padding    (rem 1)

--------------------------------------------------------------------------------

postStyle :: Css
postStyle =
  do centerColumn

     ".post__header" ?
       do centerColumn

     ".post__title" ?
       do titleFont
          textAlign center

     ".post__body" ?
       do maxWidth         postWidth

     ".post__body" |> p # firstChild # firstLetter?
       do -- Create a drop-cap effect
          fontFamily    ["Times"] [serif]
          float         floatLeft
          fontSize      (px 70)
          lineHeight    (px 50)
          marginBottom  (px $ -4)
          padding       (px 4) (px 8) (px 0) (px 0)

--------------------------------------------------------------------------------

postlistStyle :: Css
postlistStyle =
  do -- organize our content into three rows
     display                        grid
     "grid-template-columns"     -: "10fr 5fr 6fr"
     "grid-gap"                  -: "1em"

     ".postlist__date" ?
       do "grid-column"          -: "2"

     ".postlist__title" ?
       do "grid-column"          -: "1"


     ".postlist__tags" ?
       do "grid-column"          -: "3"

     -- Remove tags on small screens
     query all [M.maxWidth $ px 666] $
       do ".postlist__tags" ?
            do display  none

--------------------------------------------------------------------------------

tagStyle :: Css
tagStyle =
  do ".tag" ?
       do headerFont
          fontSize        (rem 0.9)
          textTransform   uppercase
          textDecoration  none
          marginRight     (px 14)
          display         inlineBlock

          not ".tag--icon" & before &
            do content          (stringContent "")
               backgroundImage  (url "/images/icons/tag.svg")
               backgroundSize   cover
               display          inlineBlock
               width            (rem 0.8)
               height           (rem 0.9)
               paddingRight     (px 3)
               marginBottom     (px $ -2)

     -- Maintain the same color for tag icons
     ".tag--icon" ** img ?
       do filters               [ F.invert $ pct 100
                                , F.url "#tag-color-filter" ]

     -- Support Dark Mode

     ".dark" ** ".tag--icon" ** img ?
       do filter                (F.url "#tag-color-filter")


     ".dark" ** ".tag" # not ".tag--icon" # before ?
       do filter  (invert $ pct 100)

     ".dark" ** ".tag--icon" ?
       do filter  (invert $ pct 100)


     -- Enlarge a tag in a title.

     ".post__title .tag" ?
       do fontSize        (rem 4)

          not ".tag--icon" & before &
            do width (rem 3)
               height (rem 3)

          img ?
            do width (rem 6)

--------------------------------------------------------------------------------

tagcloudStyle :: Css
tagcloudStyle = do
  sym2 margin    (px 0) auto
  maxWidth       (px 500)
  textAlign      center
  fontSize       (em 3)

  a ?
    do headerFont
       textDecoration  none
       textTransform   uppercase
       lineHeight      (unitless 1)

--------------------------------------------------------------------------------

pageFooterStyle :: Css
pageFooterStyle =
  do sym padding      (em 1)
     sym2 margin      nil (em 0.5)
     centerColumn
     textFont

     ".page-footer__wrapper" ?
       do maxWidth   postWidth
          width      (pct 100)
          textAlign  center
--------------------------------------------------------------------------------

darkModeButtonStyle :: Css
darkModeButtonStyle =
  do ".dark-mode" ?
       do position       absolute
          top            nil
          right          nil

          -- Reset button styles
          border          solid 0 transparent
          sym padding     (px 10)
          sym margin      nil
          cursor          pointer


          backgroundColor      bgColorDark
          color                txtColorDark


     -- Allow for dark mode
     ".dark" ** ".dark-mode" ?
       do backgroundColor      bgColor
          color                txtColor
