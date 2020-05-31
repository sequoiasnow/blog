{-# LANGUAGE OverloadedStrings #-}

module Err.SyntaxHighlight where

import Clay

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
  ".st" ? color "FFE9AA" -- String
  ".va" ? color "FFE9AA" -- Variable
  ".vs" ? color "#FFE9AA" -- VerbatimString
  ".wa" ? color "FFE9AA" -- Warning

lightTheme :: Css
lightTheme = do
  ".al" ? color "" -- Alert
  ".an" ? color "" -- Annotation
  ".at" ? color "" -- Attribute
  ".bn" ? color "" -- BaseN
  ".bu" ? color "" -- BuiltIn
  ".cf" ? color "" -- ControlFlow
  ".ch" ? color "" -- Char
  ".cn" ? color "" -- Constant
  ".co" ? color "" -- Comment
  ".cv" ? color "" -- CommentVar
  ".do" ? color "" -- Documentation
  ".dt" ? color "" -- DataType
  ".dv" ? color "" -- DecVal
  ".er" ? color "" -- Error
  ".ex" ? color "" -- Extension
  ".fl" ? color "" -- Float
  ".fu" ? color "" -- Function
  ".im" ? color "" -- Import
  ".in" ? color "" -- Information
  ".kw" ? color "" -- Keyword
  ".op" ? color "" -- Operator
  ".ot" ? color "" -- Other
  ".pp" ? color "" -- Preprocessor
  ".re" ? color "" -- RegionMarker
  ".sc" ? color "" -- SpecialCharacter
  ".ss" ? color "" -- SpecialString
  ".st" ? color "" -- String
  ".va" ? color "" -- Variable
  ".vs" ? color "" -- VerbatimString
  ".wa" ? color "" -- Warning
