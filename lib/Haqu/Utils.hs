{-# LANGUAGE OverloadedStrings #-}

module Haqu.Utils where
import Data.Maybe (fromMaybe)

split :: Char -> String -> [String]
split c xs = case break (==c) xs of
    (ls, "") -> [ls]
    (ls, _:rs) -> ls : split c rs

unwrapMaybe :: Maybe a -> a
unwrapMaybe = fromMaybe (error "Cannot unwrap Nothing")

removeExtension :: String -> String
removeExtension = takeWhile (/= '.')