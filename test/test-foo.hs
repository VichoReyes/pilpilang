{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Exit (exitFailure)
import Text.Megaparsec (runParser)
import Types (Parser, parseBool)

main = do
    if runParser parseBool "" "true" == Right True
      then return ()
      else exitFailure
