{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Exit (exitFailure)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Either (isLeft, isRight)
import Syntax
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
    parseCheck (pQuotedLiteral True) "\"escapedquote\\\"here\"" "escapedquote\"here"
    parseCheck pActor "actor Actor { table \"tablename\" }" Actor
        {actorName = "Actor", actorTable = "tablename", actorColumns = Nothing}
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
                permissionActor = OptTypeVar {otvarName = "actor", otvarType = Just "Actor"}, 
                permissionResource = OptTypeVar {otvarName = "resource", otvarType = Just "Doc"}
                }
            ), 
        assocDefinition = PEquals (VLiteral 7) (VVarField "actor" "age")}

complexCase2 :: IO ()
complexCase2 = parseCheck pAssoc "can_write(actor, resource: Doc) if hola(asdfasd) && 7 = 5" 
    Assoc { 
        assocHeader = AHPermission (
            Permission {
                permissionType = PCanWrite, 
                permissionActor = OptTypeVar {otvarName = "actor", otvarType = Nothing}, 
                permissionResource = OptTypeVar {otvarName = "resource", otvarType = Just "Doc"}
                }
            ), 
        assocDefinition = PAnd
            (PCall $ PredCall "hola" ["asdfasd"])
            (PEquals (VLiteral 7) (VLiteral 5))
            }

complexCase3 :: IO ()
complexCase3 = parseCheck pAssoc "numbers(a, r: T) if a >4   || (r   <8   && a  =  r)" 
    Assoc { 
        assocHeader = AHDef (
            Definition {
                defName = "numbers", 
                defArgs = [OptTypeVar "a" Nothing, OptTypeVar "r" (Just "T")]
                }
            ), 
        assocDefinition = POr
            (PGreaterT (VVar "a") (VLiteral 4))
            (PAnd
                (PLessT (VVar "r") (VLiteral 8))
                (PEquals (VVar "a") (VVar "r")))
            }

