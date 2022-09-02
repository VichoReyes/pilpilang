{-# LANGUAGE OverloadedStrings #-}

module TypesSpec where

import Types
import Syntax
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (ord)
import Test.Hspec (Spec)

data MockDB

instance ColumnTypeProvider MockDB where
    fillTypes _ = fillColumnTypes

-- very much mock
fillColumnTypes :: GEntity a Text -> IO (GEntity a (Text, Text))
fillColumnTypes (Entity name table cols) = do
    return $ Entity name table typedCols
        where
            typedCols = do
                untypedCol <- cols
                let colType = case T.uncons untypedCol of
                                   Just (c, _) -> typeFrom c
                                   Nothing -> undefined
                return (untypedCol, colType)
            typeFrom c = case ord c `mod` 3 of
                0 -> "Int"
                1 -> "Bool"
                2 -> "String"
                _ -> undefined

spec :: Spec
spec = do
    return ()
