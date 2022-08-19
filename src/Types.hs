{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text as T
import Data.Void (Void)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (digitToInt)

type Parser = Parsec Void T.Text

