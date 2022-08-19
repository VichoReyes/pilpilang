{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Exit (exitFailure)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP.C
import Data.Either (isLeft)
import Types (Parser)
import qualified Data.Text as T

parsePass :: (Eq a) => Parser a -> T.Text -> a -> IO ()
parsePass parser text result = do
    if MP.runParser (parser <* MP.eof) "" text == Right result
      then return ()
      else exitFailure

parseFail :: Parser a -> T.Text -> IO ()
parseFail parser text = do
    if isLeft $ MP.runParser (parser <* MP.eof) "" text
      then return ()
      else exitFailure

main = do
    parsePass (MP.C.char 'a') "a" 'a'
    parsePass (MP.C.string' "hola") "HoLa" "HoLa"
    parseFail (MP.C.string' "hola") "HoLaa"
