module Lexer where

import           Control.Lens
import           Control.Monad.State

lex :: String -> [String]
lex = recurse "" where
  recurse wrk "" =
    case wrk of
      "" -> []
      _  -> [wrk]
  recurse wrk str@(c:str') =
    case extractDelimeter str of
      Just ((Just del), str') ->
        case wrk of
          "" -> del : recurse "" str'
          _  -> wrk : del : recurse "" str'
      Just (Nothing, str') ->
        case wrk of
          "" -> recurse "" str'
          _  -> wrk : recurse "" str'
      Nothing -> recurse (wrk++[c]) str'

delimeters :: [String]
delimeters = spacers ++ reserveds

spacers :: [String]
spacers =
  [ " ", "\n" ]

reserveds :: [String]
reserveds =
  [ "(", ")"
  , "=", "<"
  , "+", "-"
  , "*", "div", "mod"
  , "**"
  , "not"
  ]

extractDelimeter :: String -> Maybe (Maybe String, String)
extractDelimeter str =
  case extractFirst spacers str of
    Just (_, str') -> Just (Nothing, str')
    Nothing ->
      case extractFirst reserveds str of
        Just (r, str') -> Just (Just r, str')
        Nothing        -> Nothing

extractFirst :: [String] -> String -> Maybe (String, String)
extractFirst ss str = recurse ss where
  recurse [] = Nothing
  recurse (s:ss') =
    case extract s str of
      Just str' -> Just str'
      Nothing   -> recurse ss'

extract :: String -> String -> Maybe (String, String)
extract pre str = recurse pre str >>= \str' -> Just (pre, str') where
  recurse "" str = Just str
  recurse rpe "" = Nothing
  recurse (x:pre') (y:str') =
    if x == y then recurse pre' str' else Nothing
