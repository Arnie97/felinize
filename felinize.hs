#!/usr/bin/env runhaskell

import Data.Char (isAlpha, isPunctuation, isSpace)
import Data.List (dropWhile, dropWhileEnd, isPrefixOf)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

main =
  interact
    $ unlines
    . fmap (\line -> line !! 0)
    . filter (numberLocation $ isPrefixOf "北京市")
    . filter (numberType $ (==) "MOBILE")
    . fmap (fmap trim . splitOn ";")
    . lines
 where
  numberLocation cond line = cond $ last line
  numberType     cond line = cond $ line !! 2

trim = dropWhileEnd shouldTrim . dropWhile shouldTrim
  where shouldTrim s = isSpace s || s == '"'
