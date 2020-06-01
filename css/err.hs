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

    -- Overrides for print
    query M.print [] $
      do -- Force light mode
         "body.dark" ?
           do backgroundColor bgColor
              color           txtColor
              borderColor     txtColor

         ".dark" ** star ?
           do borderColor    txtColor

         -- Set the correct width for the paper
         body <> html ?
           do backgroundColor white
              width           (px 960)

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

    nav            ? navStyle
    ".content"     ? contentStyle
    ".post"        ? postStyle
    ".postlist"    ? postlistStyle
    ".tag-cloud"   ? tagcloudStyle
    ".page-footer" ? pageFooterStyle

    -- Styles for elements common to various areas
    tagStyle

--------------------------------------------------------------------------------

-- Color Pallet

bgColor, txtColor, emphColor, emphColor2, tagColor :: Color

bgColor      = lighten 1 "#f9f7f4"
txtColor     = "#000"
emphColor    = "#807FE0"
emphColor2   = "#FF5257"
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

textFont, headerFont, codeFont, titleFont, altFont :: Css

textFont =
  do fontSize    (rem 0.9)
     fontFamily  ["Courier Prime", "Monaco", "-apple-system"] [monospace]
     fontWeight  (weight 400)
     lineHeight  (unitless 1.618)

altFont = textFont >> fontStyle italic

headerFont =
  do fontFamily  ["IBM Plex Mono", "Monaco", "-apple-system"] [monospace]
     fontWeight (weight 700)

titleFont =
  do headerFont
     fontSize   (em 2)

codeFont =
  do fontFamily ["Monaco", "Fira Mono", "Source Sans Serif"] [monospace]

--------------------------------------------------------------------------------

-- Layout constraints for the site.

siteWidth, postWidth, contentWidth :: Size LengthUnit

siteWidth    = px 960
postWidth    = px 640
contentWidth = px 720


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
      do width           (pct 90)
         maxWidth        (vw 90)
         borderTop       solid (px 1) inherit
         borderBottom    solid nil black
         borderLeft      solid nil black
         borderRight     solid nil black
         sym2 margin     (em 1) nil

    -- Center figures, with descriptive text intentionally separated.

    figure ?
      do display          flex
         flexDirection    column
         alignItems       center
         maxWidth         (vw 100)
         sym2 margin      (em 1) auto

         img ?
           sym margin (px 10)

         figcaption ?
           do sym2 margin    (rem 0.3) (em 4)
              textAlign       end
              width           (pct 70)
              borderTopStyle  solid
              borderTopWidth  (px 1)
              altFont

    -- Modern looking blockquotes, with the classic left bar and margin
    blockquote ?
      do sym2 margin    (em 2) (em 3)
         altFont
         fontStyle      italic
         maxWidth       (vw 100)


    --Style our code blocks with some line numbers and themes.

    pre ?
      do sym padding    (rem 1)
         codeFont
         maxWidth       (vw 100)
         width          (pct 100)
         whiteSpace     preWrap
         wordBreak      breakAll

         -- Default font style is light
         doomSolarized

    ".dark" ** pre ? doomChallengerDeep

    -- Handle printing
    query M.print [] $ pre ?
      do doomSolarized
         backgroundColor    transparent
         filter             (grayscale $ pct 50)

    -- Some simple classes for writing posts
    ".center" ?
      do display         flex
         flexDirection   column
         justifyContent  center

    ".Huge-text" ?
      do fontSize (em 3)
         textAlign center




--------------------------------------------------------------------------------

navStyle :: Css
navStyle =
  do -- Set the general styling of the header.
     sym2 padding    0 (em 0.6)
     marginRight     (em 0.5)

     ul ?
       do paddingLeft     nil
          marginTop       nil
          display         flex
          flexDirection   column
          listStyleType   none


     ".nav__title" ?
       do whiteSpace        nowrap
          borderBottomStyle solid
          borderBottomWidth (px 1)
          fontSize          (em 1.8)

          ".factorial" ?
            do color  emphColor2

     ".nav__pages" ?
       do borderBottomStyle  dashed
          borderBottomWidth  (px 1)

     a ?
       do sym2 margin    (px 5) 0
          transition     "all" (sec 0.2) ease (sec 0)
          textDecoration none
          color          inherit
          headerFont

          hover &
            color        emphColor

     ".nav__tags" <> ".nav__pages" ?
       do marginTop      (em 0.5)
          paddingBottom  (em 0.5)


     query all [M.maxWidth $ px 666] $
       do borderBottomStyle solid
          borderBottomWidth (px 1)
          marginBottom      (em 1)
          width             (pct 100)

     -- Kill the navigation for print
     query M.print [] $ display none


--------------------------------------------------------------------------------

contentStyle :: Css
contentStyle =
  do -- Set common styles to the display of a post.
     textFont
     maxWidth        contentWidth
     width           (pct 100)
     sym2 padding    (em 0.2) (em 0.6)



--------------------------------------------------------------------------------

postStyle :: Css
postStyle =
  do centerColumn

     ".post__header" ?
       do centerColumn

     ".post__title" ?
       do titleFont
          textAlign     center
          marginBottom  (em 0.5)

     ".post__body" ?
       do maxWidth         postWidth

     ".post__tags" ?
       do marginBottom  (em 1.5)

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
       do "grid-template-columns"     -: "10fr 5fr"

          ".postlist__tags" ?
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
       do fontSize        (rem 2)

          not ".tag--icon" & before &
            do width   (rem 1.7)
               height  (rem 1.7)

          img ?
            do width (rem 3)

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
  do sym2 padding     (em 0.2) (em 2)
     sym2 margin      nil (em 0.5)
     centerColumn
     borderTopStyle   solid
     borderTopWidth   (px 1)
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

          -- Hide on print
          query M.print [] $ display none


     -- Allow for dark mode
     ".dark" ** ".dark-mode" ?
       do backgroundColor      bgColor
          color                txtColor

--------------------------------------------------------------------------------

-- Color Schemes

doomChallengerDeep :: Css
doomChallengerDeep = do
  -- background
  backgroundColor  "#1E1C31"
  color            "#CBE3E7"

  ".al" ? color "#FF8080" -- Alert
  ".an" ? color "#65B2FF" -- Annotation
  ".at" ? color "#91DDFF" -- Attribute
  ".bn" ? color "#CBE3E7" -- BaseN
  ".bu" ? color "#C991E1" -- BuiltIn
  ".cf" ? color "#91DDFF" -- ControlFlow
  ".ch" ? color "#FFE9AA" -- Char
  ".cn" ? color "#FFE9AA" -- Constant
  ".co" ? color "#65B2FF" -- Comment
  ".cv" ? color "#565575" -- CommentVar
  ".do" ? color (darken 30 "#62D196") -- Documentation
  ".dt" ? color "#FF8080" -- DataType
  ".dv" ? color "#FFE9AA" -- DecVal
  ".er" ? color "#FF8080" -- Error
  ".ex" ? color "#C991E1" -- Extension
  ".fl" ? color "#FFB378" -- Float
  ".fu" ? color "#C991E1" -- Function
  ".im" ? color "#FF8080" -- Import
  ".in" ? color "#CBE3E7" -- Information
  ".kw" ? color "#FF8080" -- Keyword
  ".op" ? color "#63F2F1" -- Operator
  ".ot" ? color "#CBE3E7" -- Other
  ".pp" ? color "#63F2F1" -- Preprocessor
  ".re" ? color "#906CFF" -- RegionMarker
  ".sc" ? color "#FFE9AA" -- SpecialCharacter
  ".ss" ? color "#FFE9AA" -- SpecialString
  ".st" ? color "#FFE9AA" -- String
  ".va" ? color "#FFE9AA" -- Variable
  ".vs" ? color "#FFE9AA" -- VerbatimString
  ".wa" ? color "#FFE9AA" -- Warning

doomSolarized :: Css
doomSolarized = do
  -- background
  backgroundColor  "#FDF6E3"
  color            "#556b72"

  ".al" ? color "#dc322f" -- Alert
  ".an" ? color "#b58900" -- Annotation
  ".at" ? color "#2aa198" -- Attribute
  ".bn" ? color "#6c71c4" -- BaseN
  ".bu" ? color "#859900" -- BuiltIn
  ".cf" ? color "#FCF8ED" -- ControlFlow
  ".ch" ? color "#2aa198" -- Char
  ".cn" ? color "#6c71c4" -- Constant
  ".co" ? color "#96A7A9" -- Comment
  ".cv" ? color "#788484" -- CommentVar
  ".do" ? color (lighten 20 "#35a69c") -- Documentation
  ".dt" ? color "#859900" -- DataType
  ".dv" ? color "#859900" -- DecVal
  ".er" ? color "#dc322f" -- Error
  ".ex" ? color "#FCF8ED" -- Extension
  ".fl" ? color "#6c71c4" -- Float
  ".fu" ? color "#d33682" -- Function
  ".im" ? color "#268bd2" -- Import
  ".in" ? color "#96A7A9" -- Information
  ".kw" ? color "#859900" -- Keyword
  ".op" ? color "#268bd2" -- Operator
  ".ot" ? color "#b58900" -- Other
  ".pp" ? color "#268bd2" >> fontWeight bold -- Preprocessor
  ".re" ? color  (darken 20 "#FFFBEA") -- RegionMarker
  ".sc" ? color "#FFE9AA" -- SpecialCharacter
  ".ss" ? color "#b58900" -- SpecialString
  ".st" ? color "#2aa198" -- String
  ".va" ? color "#268bd2" -- Variable
  ".vs" ? color "#6c71c4" -- VerbatimString
  ".wa" ? color "#b58900" -- Warning
