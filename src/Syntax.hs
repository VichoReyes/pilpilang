{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Syntax where

import Lens.Micro.Platform ( makeLenses )
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import qualified Data.Map as M
import Common

type Parser = Parsec Void Text

-- Skipper of whitespace. Accepts line comments with '#'
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "#") empty

-- Use this function to skip whitespace after a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

data AST = AST
    { _astEntities :: [Entity]
    , _astAssociations :: [Assoc]
    } deriving (Eq, Show, Ord)

pAST :: Parser AST
pAST = do
    spaceConsumer -- initial discarding of whitespace
    _astEntities <- some (lexeme pEntity)
    _astAssociations <- some (lexeme pAssoc)
    return AST{..}

type Entity = GEntity Text Text

-- run a function for columns with keys and another one for columns without
columnsMap :: ([Text] -> a -> b) -> (c -> d) -> GEntity a c -> GEntity b d
columnsMap f g ent = ent {_entityColumns = fmap go (_entityColumns ent)}
    where
        go colType =
            let colType' = case colType of
                    Left (foreignEnt, keys) -> Left (f keys foreignEnt, keys)
                    Right primitive -> Right (g primitive)
             in colType'

type BasicColumn = ColumnType Text Text

stringLiteral :: Parser Text
stringLiteral = char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

pEntity :: Parser Entity
pEntity = do
    _entityClass <- choice
        [ EActor <$ pKeyword "actor"
        , EResource <$ pKeyword "resource" ]
    _entityName <- pTitleCasedWord
        <?> "actor name (should start with upper case letter)"
    symbol "{"
    symbol "table" <?> "table (which table stores "<>T.unpack _entityName<>"s?)"
    _entityTable <- pQuotedLiteral False <?> "table name"
    _entityColumns <- M.fromList <$> pColumnsList
    symbol "keys"
    _entityKeys <- symbol "[" *> pCommaSepList (pQuotedLiteral False) <* symbol "]"
    symbol "}"
    return $ GEntity {..}

pColumnsList :: Parser [(Text, BasicColumn)]
pColumnsList = go <|> return []
    where
        go = do
            symbol "columns"
            symbol "["
            columns <- pCommaSepList pTypedColumn
            symbol "]"
            return columns
        pTypedColumn = do
            colName <- pLowerCasedWord <|> pQuotedLiteral False
            symbol ":"
            colType <- pTitleCasedWord
            foreignKeys <- optional $ symbol "(" *> pCommaSepList (pQuotedLiteral False) <* symbol ")"
            let theType = case foreignKeys of
                    Nothing -> Right colType
                    Just foreignKeys' -> Left (colType, foreignKeys')
            return (colName, theType)

pTitleCasedWord :: Parser Text
pTitleCasedWord = lexeme $ do
    first <- upperChar
    rest <- many (alphaNumChar <|> char '_')
    return $ T.pack (first:rest)

pLowerCasedWord :: Parser Text
pLowerCasedWord = lexeme $ do
    first <- lowerChar
    rest <- many (alphaNumChar <|> char '_')
    return $ T.pack (first:rest)

-- Combinator like `many` but with interspersed commas.
-- The final comma is optional.
pCommaSepList :: Parser a -> Parser [a]
pCommaSepList pElem = pCommaSepList2 <|> pure []
    where
        pCommaSepList2 = do
            el <- pElem
            canContinue <- True <$ symbol "," <|> pure False
            if canContinue
                then (el : ) <$> pCommaSepList pElem
                else return [el]

pQuotedLiteral :: Bool -> Parser Text
pQuotedLiteral allowEmpty = lexeme $ do
    char '"'
    prefix <- if allowEmpty
        then return ""
        else (:[]) <$> L.charLiteral
    literal <- manyTill L.charLiteral (char '"')
    return $ T.pack (prefix <> literal)

type Assoc = GAssoc Text ()

pAssoc :: Parser Assoc
pAssoc = lexeme $ do
    _assocHeader <- pAssocHeader
    symbol "if"
    _assocDefinition <- pPredicate
    return GAssoc {..}

type AssocHeader = GAssocHeader Text

pAssocHeader :: Parser AssocHeader
pAssocHeader = lexeme $
    (Left <$> pPermission) <|> (Right <$> pDefinition)

type Permission = GPermission Text

pPermission :: Parser Permission
pPermission = lexeme $ do
    _permissionType <- pPermissionType
    symbol "("
    _permissionActor <- pTypedVar
    symbol ","
    _permissionResource <- pTypedVar
    symbol ")"
    return GPermission {..}

pPermissionType :: Parser PermissionType
pPermissionType = lexeme $ choice
    [ PCanSelect <$ pKeyword "can_select"
    , PCanInsert <$ pKeyword "can_insert"
    , PCanUpdate <$ pKeyword "can_update"
    , PCanDelete <$ pKeyword "can_delete"
    ]

pTypedVar :: Parser (Text, Text)
pTypedVar = lexeme $ do
    a <- pLowerCasedWord
    symbol ":"
    b <- pTitleCasedWord <?> "type (an actor or resource)"
    return (a, b)

type Definition = GDefinition Text

pDefinition :: Parser Definition
pDefinition = lexeme $ do
    _defName <- pLowerCasedWord
    symbol "("
    _defArgs <- pCommaSepList pTypedVar
    symbol ")"
    return GDefinition {..}

type Predicate = GPredicate ()

pPredicate :: Parser Predicate
pPredicate = makeExprParser pTerm operatorTable
    where
        pTerm = choice
            [ try pPredCall
            -- TODO: Do this with makeExprParser too
            , PEquals <$> try (pValue <* symbol "=") <*> pValue
            , PAlways <$ string "always"
            , PGreaterT <$> try (pValue <* symbol ">") <*> pValue
            , PLessT <$> try (pValue <* symbol "<") <*> pValue
            , between (symbol "(") (symbol ")") pPredicate
            ]
        operatorTable = [
            [ InfixL (PAnd <$ symbol "&&")
            , InfixR (POr <$ symbol "||")
            ]]

pPredCall :: Parser Predicate
pPredCall = do
    name <- pLowerCasedWord
    symbol "("
    args <- pCommaSepList pValue
    symbol ")"
    return $ PredCall name args

type Value = GValue ()

pValue :: Parser Value
pValue = lexeme $ choice
    [ VLiteral . LitInt <$> L.signed empty L.decimal
    , VLiteral . LitString <$> pQuotedLiteral True
    , VLiteral (LitBool True) <$ string "true"
    , VLiteral (LitBool False) <$ string "false"
    , flip VVar () <$> pLowerCasedWord
    ] >>= pValueField

pValueField :: Value -> Parser Value
pValueField val = do
    field <- optional $ do
        char '.'
        pLowerCasedWord
    case field of
        Nothing -> return val
        Just fieldName -> pValueField $ VVarField val fieldName ()


-- esperado: (VVF (VVF (VV abc) def) ghi)
-- abc.def.ghi

makeLenses ''AST
