#!/usr/bin/env runhaskell

import Data.Char (chr, ord, isAlpha, isPunctuation, isSpace)
import Data.List (dropWhile, dropWhileEnd, isPrefixOf)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

main = interact $
    unlines .
    fmap (\line -> felinize [] False [] 0 (line !! 0)) .
    filter (numberLocation (isPrefixOf "北京市")) .
    filter (numberType ((==) "MOBILE")) .
    fmap (fmap trim . splitOn ";") .
    lines
    where numberLocation cond line = cond $ last line
          numberType     cond line = cond $ line !! 2

-- >>> trim "  \"hello world\"  \n"
-- "hello world"
trim = dropWhileEnd shouldTrim . dropWhile shouldTrim
    where shouldTrim s = isSpace s || s == '"'

-- >>> expand [] "PN-LGF-CA"
-- "ACDEFGLMNP"
expand :: String -> String -> String
expand (y:ys) ('-':x:xs)
    | x == y    = expand (y:ys) xs
    | otherwise = expand (chr (ord y - 1):y:ys) ('-':x:xs)
expand ys (x:xs) = expand (x:ys) xs
expand ys [] = ys
