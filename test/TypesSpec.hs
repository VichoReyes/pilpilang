{-# LANGUAGE OverloadedStrings #-}

module TypesSpec where

import Types
import Syntax
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (ord)
import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.State (execStateT)
import qualified Data.Map as M

data MockDB

instance ColumnTypeProvider MockDB where
    fillTypes _ = fillColumnTypes

-- very much mock
fillColumnTypes :: GEntity a Text -> Either TypeError (GEntity a (Text, Text))
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

completeTypeInfo :: TypeInfo
completeTypeInfo = TypeInfo 
    { actors = M.fromList [("A", M.fromList [("something", "Int"), ("else", "String")])]
    , resources = M.empty
    , functions = M.empty
    }

actorA ::TypedActor
actorA = Entity "A" undefined [("something", "Int"), ("else", "String")]

spec :: Spec
spec = do
    describe "mkTypeInfo" $
        it "works" $
            execStateT (mkTypeInfo [actorA] []) emptyTypeInfo
                `shouldBe` Right completeTypeInfo
