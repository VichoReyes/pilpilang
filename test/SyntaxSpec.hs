{-# LANGUAGE OverloadedStrings #-}

module SyntaxSpec (main, spec) where

import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec
import Syntax
import Data.Text (Text)
import qualified Data.Text.IO as TIO

main :: IO ()
main = hspec spec

actor1 :: Actor
actor1 = actor "MyActor" "actors" ["col1", "col2"]

actor1_desc :: Text
actor1_desc = "actor MyActor {\
\    table \"actors\"\
\    columns [\"col1\", \"col2\"] \
\ }"

spec :: Spec
spec = do
    describe "pActor" $ do
        it "requires whitespace after symbol" $
            parse pActor "" `shouldFailOn` "actorActor { table \"tablename\" columns []}"
        it "parses correct descriptions" $ do
            parse pActor "" `shouldSucceedOn` "actor Actor { table \"tablename\" columns []}"
            parse pActor "" actor1_desc `shouldParse` actor1
        it "parses an actor without columns" $
            parse pActor "" "actor Actor { table \"tablename\" }" `shouldParse`
                actor "Actor" "tablename" []
        it "handles underscores" $
            parse pActor "" `shouldSucceedOn` "actor Actor_1 {table \"asdf\" }"
        it "disallows empty table names" $
            parse pActor "" `shouldFailOn` "actor Ac { table \"\" }"

    describe "pResource" $
        it "works" $
            parse pResource "" `shouldSucceedOn` "resource Re {table \"asdf\"}"

    describe "pQuotedLiteral" $
        it "handles escaped quotes" $
            parse (pQuotedLiteral True) "" "\"escapedquote\\\"here\"" `shouldParse` "escapedquote\"here"

    describe "pAssoc" $ do
        it "works" $
            parse pAssoc "" "can_write(actor: Actor, resource: Doc) if 7 = actor.age" `shouldParse`
                Assoc {
                    assocHeader = AHPermission (
                        Permission {
                            permissionType = PCanWrite,
                            permissionActor = TypedVar {typedVarName = "actor", typedVarType = "Actor"},
                            permissionResource = TypedVar {typedVarName = "resource", typedVarType = "Doc"}
                            }
                        ),
                    assocDefinition = PEquals (VLitInt 7) (VVarField "actor" "age")}
        it "handles AND predicates (&&)" $
            parse pAssoc "" "can_write(actor: T, resource: Doc) if hola(asdfasd) && \"cinco\" = 5" `parseSatisfies`
                (\assoc -> case assocDefinition assoc of
                    (PAnd (PCall _) (PEquals _ _)) -> True
                    _ -> False)
        it "handles OR predicates (||)" $
            parse pAssoc "" "numbers(a: A, r: T) if a >4   || (r   <8   && a  =  r)" `parseSatisfies`
                (\assoc -> case assocDefinition assoc of
                    (POr (PGreaterT _ _) (PAnd (PLessT _ _) (PEquals _ _))) -> True
                    _ -> False)

    describe "pAST" $
        it "parses the twitter example" $
            TIO.readFile "test/examples/twitter.pilpil" >>= (parse pAST "" `shouldSucceedOn`)
