{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Exit (exitFailure)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Either (isLeft, isRight)
import Types
import qualified Data.Text as T

-- TODO: usar hspec-megaparsec

parseCheck :: (Eq a) => Parser a -> T.Text -> a -> IO ()
parseCheck parser text result = do
    if runParser (parser <* eof) "" text == Right result
      then return ()
      else exitFailure

parseFail :: Parser a -> T.Text -> IO ()
parseFail parser text = do
    if isLeft $ runParser (parser <* eof) "" text
      then return ()
      else exitFailure

parsePass :: Parser a -> T.Text -> IO ()
parsePass parser text = do
    if isRight $ runParser (parser <* eof) "" text
      then return ()
      else exitFailure

actor1 = Actor
    { actorName = "MyActor"
    , actorTable = "actors"
    , actorColumns = Just ["col1", "col2"]
    }

actor1_desc = "actor MyActor {\
\    table \"actors\"\
\    columns [\"col1\", \"col2\"] \
\ }"

main = do
    parseFail pActor "actorActor { table \"tablename\" columns []}"
    parsePass pActor "actor Actor { table \"tablename\" columns []}"
    parseCheck pActor actor1_desc actor1
    parseCheck pQuotedLiteral "\"escapedquote\\\"here\"" "escapedquote\"here"
    parseCheck pActor "actor Actor { table \"tablename\" }" Actor
        {actorName = "Actor", actorTable = "tablename", actorColumns = Nothing}