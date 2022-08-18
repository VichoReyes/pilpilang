{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Exit (exitFailure)
import Text.Megaparsec (runParser, parseTest, eof)
import Data.Either (isLeft)
import Types (Parser, parseBool)
import qualified Data.Text as T

parsePass :: (Eq a) => Parser a -> T.Text -> a -> IO ()
parsePass parser text result = do
    if runParser (parser <* eof) "" text == Right result
      then return ()
      else exitFailure

parseFail :: Parser a -> T.Text -> IO ()
parseFail parser text = do
    if isLeft $ runParser (parser <* eof) "" text
      then return ()
      else exitFailure

main = do
    parsePass parseBool "true" True
    parseFail parseBool "asdfasdf"
