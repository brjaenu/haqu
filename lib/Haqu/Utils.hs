{-# LANGUAGE OverloadedStrings #-}

-- Module that contains general utility functions
module Haqu.Utils where
import Data.Maybe (fromMaybe)

-- Split a Char sequence at a given Char recursively
-- Returns a List of Strings that are splitted on that given Char
-- The Char c is not included in the sequences anymore
split :: Char -> String -> [String]
split c xs = case break (==c) xs of
    (ls, "") -> [ls]
    (ls, _:rs) -> ls : split c rs

-- Unwraps a Maybe Value. If the Value results to Nothing a error will be thrown
unwrapMaybe :: Maybe a -> a
unwrapMaybe = fromMaybe (error "Cannot unwrap Nothing")

-- This function removes the fileextension of a given file 
-- It supports . in base filename
removeExtension :: String -> String
removeExtension path = reverse (tail (a (reverse path)))
    where a = dropWhile (/= '.')