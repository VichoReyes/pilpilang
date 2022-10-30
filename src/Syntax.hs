{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Syntax where

import Lens.Micro.Platform
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Map (Map)
import qualified Data.Map as M

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

data GEntity withKeys without = EActor (GActor withKeys without) | EResource (GResource withKeys without)
    deriving (Eq, Ord, Show)

type Entity = GEntity Text Text

-- run a function for columns with keys and another one for columns without
-- ignoring column name
columnsMap :: ([Text] -> a -> b) -> (c -> d) -> GEntity a c -> GEntity b d
columnsMap f g = entMap
    where
        entMap (EActor a) = EActor a {_actorColumns = map go (_actorColumns a)}
        entMap (EResource r) = EResource r {_resourceColumns = map go (_resourceColumns r)}
        go (colName, colType) =
            let colType' = case colType of
                    Left (ent, keys) -> Left (f keys ent, keys)
                    Right primitive -> Right (g primitive)
             in (colName, colType')


type ColumnType withKeys without = Either (withKeys, [Text]) without

type BasicColumn = ColumnType Text Text

class HasName ent where
    nameL :: Lens' ent Text

class HasTable ent where
    tableL :: Lens' ent Text

class HasPrimaryKeys ent where
    primaryKeysL :: Lens' ent [Text]

class HasColumns withKeys without ent where
    columnsL :: Lens' ent (Map Text (ColumnType withKeys without))

data GResource withKeys without = GResource
    { _resourceName :: Text
    , _resourceTable :: Text
    , _resourceKeys :: [Text]
    , _resourceColumns :: [(Text, ColumnType withKeys without)]
    } deriving (Eq, Show, Ord)

type Resource = GResource Text Text

instance HasName (GResource a b) where
    nameL = lens _resourceName (\x y -> x {_resourceName = y})

instance HasTable (GResource a b) where
    tableL = lens _resourceTable (\x y -> x {_resourceTable = y})

instance HasPrimaryKeys (GResource a b) where
    primaryKeysL = lens _resourceKeys (\x y -> x {_resourceKeys = y})

instance (HasColumns withKeys without) (GResource withKeys without) where
    columnsL = lens (M.fromList . _resourceColumns) (\x y -> x {_resourceColumns = M.toList y})

instance (HasColumns withKeys without) (GEntity withKeys without) where
    columnsL = lens
        (\case EActor a -> (a^.columnsL) ; EResource r -> (r^.columnsL))
        (\e b -> case e of
            EActor a -> EActor (a & columnsL .~ b)
            EResource r -> EResource (r & columnsL .~ b))

instance HasName (GEntity withKeys without) where
    nameL = lens
        (\case EActor a -> (a^.nameL) ; EResource r -> (r^.nameL))
        (\e b -> case e of
            EActor a -> EActor (a & nameL .~ b)
            EResource r -> EResource (r & nameL .~ b))

instance HasPrimaryKeys (GEntity withKeys without) where
    primaryKeysL = lens
        (\case EActor a -> (a^.primaryKeysL) ; EResource r -> (r^.primaryKeysL))
        (\e b -> case e of
            EActor a -> EActor (a & primaryKeysL .~ b)
            EResource r -> EResource (r & primaryKeysL .~ b))

data GActor withKeys without = GActor
    { _actorName :: Text
    , _actorTable :: Text
    , _actorKeys :: [Text]
    , _actorColumns :: [(Text, ColumnType withKeys without)]
    } deriving (Eq, Show, Ord)

type Actor = GActor Text Text

instance HasName (GActor a b) where
    nameL = lens _actorName (\x y -> x {_actorName = y})

instance HasTable (GActor a b) where
    tableL = lens _actorTable (\x y -> x {_actorTable = y})

instance HasPrimaryKeys (GActor a b) where
    primaryKeysL = lens _actorKeys (\x y -> x {_actorKeys = y})

instance (HasColumns withKeys without) (GActor withKeys without) where
    columnsL = lens (M.fromList . _actorColumns) (\x y -> x {_actorColumns = M.toList y})

stringLiteral :: Parser Text
stringLiteral = char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

pEntity :: Parser Entity
pEntity = pKeyword "actor" *> (EActor <$> pActor) <|> pKeyword "resource" *> (EResource <$> pResource)

pResource :: Parser Resource
pResource = do
    _resourceName <- pTitleCasedWord
        <?> "resource name (should start with upper case letter)"
    symbol "{"
    symbol "table" <?> "table (which table stores "<>T.unpack _resourceName<>"s?)"
    _resourceTable <- pQuotedLiteral False <?> "table name"
    _resourceKeys <- symbol "[" *> pCommaSepList (pQuotedLiteral False) <* symbol "]"
    _resourceColumns <- pColumnsList
    symbol "}"
    return $ GResource {..}

pActor :: Parser Actor
pActor = do
    _actorName <- pTitleCasedWord
        <?> "actor name (should start with upper case letter)"
    symbol "{"
    symbol "table" <?> "table (which table stores "<>T.unpack _actorName<>"s?)"
    _actorTable <- pQuotedLiteral False <?> "table name"
    _actorKeys <- symbol "[" *> pCommaSepList (pQuotedLiteral False) <* symbol "]"
    _actorColumns <- pColumnsList
    symbol "}"
    return $ GActor {..}

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

data GAssoc headerType valueType = GAssoc
    { _assocHeader :: GAssocHeader headerType
    , _assocDefinition :: GPredicate valueType
    } deriving (Eq, Show, Ord)

type Assoc = GAssoc Text ()

pAssoc :: Parser Assoc
pAssoc = lexeme $ do
    _assocHeader <- pAssocHeader
    symbol "if"
    _assocDefinition <- pPredicate
    return GAssoc {..}

type AssocHeader = GAssocHeader Text

type GAssocHeader headerType = Either (GPermission headerType) (GDefinition headerType)

pAssocHeader :: Parser AssocHeader
pAssocHeader = lexeme $
    (Left <$> pPermission) <|> (Right <$> pDefinition)

type Permission = GPermission Text

data GPermission headerType = GPermission
    { _permissionType :: PermissionType
    , _permissionActor :: (Text, headerType)
    , _permissionResource :: (Text, headerType)
    } deriving (Eq, Show, Ord)

pPermission :: Parser Permission
pPermission = lexeme $ do
    _permissionType <- pPermissionType
    symbol "("
    _permissionActor <- pTypedVar
    symbol ","
    _permissionResource <- pTypedVar
    symbol ")"
    return GPermission {..}

data PermissionType = PCanSelect | PCanInsert | PCanUpdate | PCanDelete
    deriving (Eq, Show, Ord)

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

data GDefinition headerType = GDefinition
    { _defName :: Text
    , _defArgs :: [(Text, headerType)]
    } deriving (Eq, Show, Ord)

pDefinition :: Parser Definition
pDefinition = lexeme $ do
    _defName <- pLowerCasedWord
    symbol "("
    _defArgs <- pCommaSepList pTypedVar
    symbol ")"
    return GDefinition {..}

data GPredicate valueType
    = PredCall Text [GValue valueType]
    | PAlways
    | PAnd (GPredicate valueType) (GPredicate valueType)
    | POr (GPredicate valueType) (GPredicate valueType)
    | PEquals (GValue valueType) (GValue valueType)
    | PGreaterT (GValue valueType) (GValue valueType)
    | PLessT (GValue valueType) (GValue valueType)
    deriving (Eq, Show, Ord)

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

data GValue valueType
    = VVar
        { _valName :: Text
        , _valType :: valueType }
    | VVarField
        { _valObject :: GValue valueType
        , _valField :: Text
        , _valType :: valueType }
    | VLiteral Literal
    -- literals
    deriving (Eq, Ord)

instance Show (GValue valueType) where
    show (VVar t _) = T.unpack t
    show (VVarField v f _) = show v ++ "." ++ T.unpack f
    show (VLiteral l) = show l

data Literal
    = LitInt Int
    | LitBool Bool
    | LitString Text
    deriving (Eq, Ord, Show)

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
makeLenses ''GAssoc
makeLenses ''GEntity
makeLenses ''GValue
