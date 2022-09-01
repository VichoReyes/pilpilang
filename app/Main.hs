{-# LANGUAGE OverloadedStrings #-}

module Main where

import Syntax
import Text.Megaparsec
import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    args <- getArgs
    if (null args)
    then do
        putStrLn "Hello Pilpilang!"
        contents <- T.pack <$> getContents
        parseTest (pAssoc <* eof) contents
    else do
        case args of
            ["AST", path] -> TIO.readFile path >>= parseTest (pAST <* eof)
            ["assoc", path] -> TIO.readFile path >>= parseTest (pAssoc <* eof)
            _ -> putStrLn "bad usage"
