{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax
import Text.Megaparsec
import qualified Data.Text as T

main :: IO ()
main = do
    putStrLn "Hello Pilpilang!"
    contents <- T.pack <$> getContents
    parseTest (pAssoc <* eof) contents
