#!/usr/bin/env runhaskell

import Data.Char (isAlpha, isPunctuation, isSpace)
import Data.List (dropWhile, dropWhileEnd)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

main = interact func
  where func = unlines .
               map (nth 0) .
               filter beijing .
               filter mobilePhone .
               fmap (fmap trim . splitOn ";") .
               lines

nth n xs = xs !! n
trim = dropWhileEnd shouldTrim . dropWhile shouldTrim
  where shouldTrim s = isSpace s || s == '"'

beijing line = last line == "北京市"
mobilePhone line = line !! 2 == "MOBILE"
