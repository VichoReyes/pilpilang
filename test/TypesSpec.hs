{-# LANGUAGE OverloadedStrings #-}

module TypesSpec where

import Types
import Syntax
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (ord)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Control.Monad.State (execStateT)
import qualified Data.Map as M
import Text.Megaparsec (parse)
import Data.Either (fromRight, isLeft)

data MockDB = MockDB

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
    { entities = M.fromList [("A", (EActor, M.fromList [("something", "Int"), ("else", "String")]))]
    , functions = M.empty
    }

actorA ::TypedActor
actorA = Entity "A" undefined [("something", "Int"), ("else", "String")]

spec :: Spec
spec = do
    describe "mkTypeInfo" $ do
        it "works" $
            execStateT (mkTypeInfo [actorA] []) emptyTypeInfo
                `shouldBe` Right completeTypeInfo
        it "fails on duplicates" $
            execStateT (mkTypeInfo [actorA, actorA] []) emptyTypeInfo
                `shouldSatisfy` isLeft
    describe "typeCheckAST" $ do
        it "gets column types" $ do
            twitterExample <- TIO.readFile "test/examples/twitter.pilpil"
            let ast = fromRight undefined $ parse pAST "twitter.pilpil" twitterExample
            execStateT (typeCheckAST MockDB ast) emptyTypeInfo `shouldBe`
                Right TypeInfo {
                   entities = M.fromList [ ("User", (EActor, M.fromList [("id", "Int"), ("password", "Bool"), ("profile", "Bool"), ("username", "Int")]))
                                         , ("Tweet", (EResource, M.fromList [("contents", "Int"), ("date", "Bool"), ("user_id", "Int")]))],
                   functions = M.empty
                   }
