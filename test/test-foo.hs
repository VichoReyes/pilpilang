{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Exit (exitFailure)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (ord)
import Data.Either (isLeft, isRight)
import Syntax
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO

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

data MockDB = MockDB

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
            typeFrom c = case (ord c `mod` 3) of
                0 -> "Int"
                1 -> "Bool"
                2 -> "String"
                _ -> undefined

actor1 = actor "MyActor" "actors" ["col1", "col2"]

actor1_desc = "actor MyActor {\
\    table \"actors\"\
\    columns [\"col1\", \"col2\"] \
\ }"

main = do
    parseFail pActor "actorActor { table \"tablename\" columns []}"
    parsePass pActor "actor Actor { table \"tablename\" columns []}"
    parseCheck pActor actor1_desc actor1
    parseCheck (pQuotedLiteral True) "\"escapedquote\\\"here\"" "escapedquote\"here"
    parseCheck pActor "actor Actor { table \"tablename\" }"
        (actor "Actor" "tablename" [])
    parsePass pActor "actor Actor_1 {table \"asdf\" }"
    parseFail pActor "actor Ac {table \"\"}"
    parsePass pResource "resource Re {table \"asdf\"}"
    complexCase
    complexCase2
    complexCase3


complexCase :: IO ()
complexCase = parseCheck pAssoc "can_write(actor: Actor, resource: Doc) if 7 = actor.age" 
    Assoc { 
        assocHeader = AHPermission (
            Permission {
                permissionType = PCanWrite, 
                permissionActor = TypedVar {typedVarName = "actor", typedVarType = "Actor"},
                permissionResource = TypedVar {typedVarName = "resource", typedVarType = "Doc"}
                }
            ), 
        assocDefinition = PEquals (VLitInt 7) (VVarField "actor" "age")}

complexCase2 :: IO ()
complexCase2 = parseCheck pAssoc "can_write(actor: T, resource: Doc) if hola(asdfasd) && \"cinco\" = 5"
    Assoc { 
        assocHeader = AHPermission (
            Permission {
                permissionType = PCanWrite, 
                permissionActor = TypedVar {typedVarName = "actor", typedVarType = "T"},
                permissionResource = TypedVar {typedVarName = "resource", typedVarType = "Doc"}
                }
            ), 
        assocDefinition = PAnd
            (PCall $ PredCall "hola" [VVar "asdfasd"])
            (PEquals (VLitString "cinco") (VLitInt 5))
            }

complexCase3 :: IO ()
complexCase3 = parseCheck pAssoc "numbers(a: A, r: T) if a >4   || (r   <8   && a  =  r)"
    Assoc { 
        assocHeader = AHDef (
            Definition {
                defName = "numbers", 
                defArgs = [TypedVar "a" "A", TypedVar "r" "T"]
                }
            ), 
        assocDefinition = POr
            (PGreaterT (VVar "a") (VLitInt 4))
            (PAnd
                (PLessT (VVar "r") (VLitInt 8))
                (PEquals (VVar "a") (VVar "r")))
            }

twitterCase :: IO ()
twitterCase = do
    pilpilang <- TIO.readFile "test/examples/twitter.pilpil"
    parsePass pAST pilpilang
