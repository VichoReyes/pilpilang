{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text as T
import Data.Void (Void)
import qualified Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (digitToInt)


data PCompound = PList [PType] | PDict [(T.Text, PType)]

data PType = Primitive PPrimitive | Compound PCompound

type Parser = Parsec Void T.Text

data PPrimitive = PNumber Double | PBoolean Bool | PString T.Text

parseBool :: Parser Bool
parseBool = string "true" *> pure True <|> string "false" *> pure False

parseDigit :: Parser Int
parseDigit = token findDigit Set.empty
    where findDigit c = if '0' <= c && c <= '9'
                          then Just (digitToInt c)
                          else Nothing

parseInt :: Parser Int
parseInt = sumDecimal <$> some parseDigit
    where sumDecimal digits = sum $ zipWith (*) (reverse digits) (iterate (*10) 1)