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
import Data.Either (fromRight, isLeft, isRight)
import Control.Monad.Reader (ReaderT(runReaderT))

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
completeTypeInfo = mempty 
    { entities = M.fromList [("A", (EActor, M.fromList [("something", "Int"), ("else", "String")]))]}

actorA :: Actor
actorA = Entity "A" undefined [("something", "Int"), ("else", "String")]

sampleEnv :: TypeInfo
sampleEnv = TypeInfo { entities = M.fromList 
    [ ("User", (EActor, M.fromList [("id", "Int"), ("name", "String"), ("age", "Int")]))
    , ("Post", (EResource, M.fromList [("contents", "String"), ("private", "Bool")]))
    ]
    , functions = M.fromList [("older_than", ["User", "Int"])]
    , variables = M.fromList [("andy", "User")]
}

spec :: Spec
spec = do
    describe "mkTypeInfo" $ do
        it "works" $
            execStateT (mkTypeInfo [actorA] []) mempty
                `shouldBe` Right completeTypeInfo
        it "fails on duplicates" $
            execStateT (mkTypeInfo [actorA, actorA] []) mempty
                `shouldSatisfy` isLeft
    describe "mkGlobals" $ do
        it "gets column types" $ do
            twitterExample <- TIO.readFile "test/examples/twitter.pilpil"
            let ast = fromRight undefined $ parse pAST "twitter.pilpil" twitterExample
            (entities <$> execStateT (mkGlobals MockDB ast) mempty) `shouldBe`
                Right (M.fromList [ ("User", (EActor, M.fromList [("id", "Int"), ("password", "String"), ("profile", "String"), ("username", "String")]))
                                  , ("Tweet", (EResource, M.fromList [("contents", "String"), ("date", "Int"), ("user_id", "Int")]))])
    describe "cValue" $ do
        it "fails on non-existing variables" $
            runReaderT (cValue (VVar "fake")) sampleEnv `shouldBe` Left (TypeError "fake not found")
        it "fails on non-existing fields on variables" $
            runReaderT (cValue (VVar "andy.favorites")) sampleEnv
                `shouldBe` Left (TypeError "andy.favorites not found")
        it "works on literals, ignoring env" $ do
            runReaderT (cValue (VLitBool True)) undefined
                `shouldBe` Right "Bool"
            runReaderT (cValue (VLitString "Something")) undefined
                `shouldBe` Right "String"
            runReaderT (cValue (VLitInt 4)) undefined
                `shouldBe` Right "Int"
        it "works on var fields" $ do
            runReaderT (cValue (VVarField (VVar "andy") "age")) sampleEnv
                `shouldBe` Right "Int"
            runReaderT (cValue (VVarField (VVar "andy") "name")) sampleEnv
                `shouldBe` Right "String"
    describe "cPredicate" $ do
        it "works on predicates" $ do
            runReaderT (cPredicate (PredCall "older_than" [VVar "andy", VLitInt 18])) sampleEnv
                `shouldBe` Right ()
            runReaderT (cPredicate (PredCall "older_than" [VVarField (VVar "andy") "name", VLitInt 18])) sampleEnv
                `shouldSatisfy` isLeft
        it "val1 = val2: works on correct types" $
            runReaderT (cPredicate (PEquals (VVarField (VVar "andy") "age") (VLitInt 18))) sampleEnv
                `shouldSatisfy` isRight
        it "val1 = val2: fails on mismatched types" $
            runReaderT (cPredicate (PEquals (VVarField (VVar "andy") "name") (VLitInt 18))) sampleEnv
                `shouldSatisfy` isLeft
        it "val1 = val2: fails on matched but non-primitive types" $
            runReaderT (cPredicate (PEquals (VVar "andy") (VVar "andy"))) sampleEnv
                `shouldSatisfy` isLeft

