{-# LANGUAGE TupleSections #-}

module Token where

import           Control.Arrow

type Token = String

data TokenType = Normal | Delimeter DelimeterType

tokenTypeOf :: Token -> TokenType
tokenTypeOf tok =
  case filter (snd >>> (== tok)) delimeters of
    []            -> Normal
    (delTyp, _):_ -> Delimeter delTyp


{-
  # Delimeters
-}

data DelimeterType = Reserved | Space | Newline

delimeters :: [(DelimeterType, Token)]
delimeters = concat
  [ map (Reserved,) [ "(", ")" , "=>", "=", "<", ">", "+", "-", "**", "*", "div", "mod" , "not" ]
  , map (Space,)    [ " " ]
  , map (Newline,)  [ "\n" ] ]
