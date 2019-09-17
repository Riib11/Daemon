{-# LANGUAGE TupleSections #-}

module Token where

type Token = String

data TokenType = Normal | Delimeter DelimeterType

{-
  # Delimeters
-}

data DelimeterType = Reserved | Space | Newline

delimeters :: [(DelimeterType, Token)]
delimeters = concat
  [ map (Reserved,) [ "(", ")" , "=", "<", "+", "-", "**", "*", "div", "mod" , "not" ]
  , map (Space,)    [ " " ]
  , map (Newline,)  [ "\n" ] ]
