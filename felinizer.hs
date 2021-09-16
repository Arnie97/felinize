#!/usr/bin/env runhaskell

import Data.Char (chr, ord, isAlpha, isPunctuation, isSpace)
import Data.List (dropWhile, dropWhileEnd, intercalate, isPrefixOf)
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

-- >>> felinize [] False [] 0 "18528"
-- "18528?d?d?d?d?d?d"
-- >>> felinize [] False [] 0 "13269[1-3589][0-35-9]"
-- "123589,012356789,13269?1?2?d?d?d?d"
felinize :: [String] -> Bool -> String -> Int -> String -> String
felinize ranges False pattern 11  [] =
    intercalate "," (reverse (pattern : ranges))
felinize ranges _ pattern patternLength [] =
    felinize ranges False (pattern ++ "?d") (patternLength + 1) []
felinize ranges False pattern patternLength ('[':xs) =
    felinize ([]:ranges) True (pattern ++ '?' : (show . (+) 1 . length) ranges) (patternLength + 1) xs
felinize (r:rs) True  pattern patternLength (']':xs) =
    felinize (expand [] r:rs) False pattern patternLength xs
felinize (r:rs) True  pattern patternLength (x:xs) =
    felinize ((x:r):rs) True pattern patternLength xs
felinize ranges False pattern patternLength (x:xs) =
    felinize ranges False (pattern ++ [x]) (patternLength + 1) xs
