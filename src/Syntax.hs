{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Syntax where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

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
    { astActors :: [Actor]
    , astResources :: [Resource]
    , astAssociations :: [Assoc]
    } deriving (Eq, Show, Ord)

pAST :: Parser AST
pAST = do
    spaceConsumer -- initial discarding of whitespace
    astActors <- some (lexeme pActor)
    astResources <- some (lexeme pResource)
    astAssociations <- some (lexeme pAssoc)
    return AST{..}

-- Basically, there are 4 possibilities for entities:
-- 1. actors without types (entityClass = ActorMarker, columnType = Text)
-- 2. actors with types (entityClass = ActorMarker, columnType = (Text, Text))
-- 1. resources without types (entityClass = ResourceMarker, columnType = Text)
-- 2. resources with types (entityClass = ResourceMarker, columnType = (Text, Text))
data GEntity entityClass columnType = Entity
    { entityName :: Text
    , entityTable :: Text
    , entityColumns :: [columnType]
    } deriving (Eq, Show, Ord)

data ActorMarker
type Actor = GEntity ActorMarker (Text, Text)

data ResourceMarker
type Resource = GEntity ResourceMarker (Text, Text)

-- constructors
actor :: Text -> Text -> [a] -> GEntity ActorMarker a
resource :: Text -> Text -> [a] -> GEntity ResourceMarker a
actor = Entity
resource = Entity

stringLiteral :: Parser Text
stringLiteral = char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

pActor :: Parser Actor
pActor = pEntity "actor"

pResource :: Parser Resource
pResource = pEntity "resource"

pEntity :: Text -> Parser (GEntity entityClass (Text, Text))
pEntity keyword = do
    pKeyword keyword
    entityName <- pTitleCasedWord
        <?> T.unpack keyword <> " name (should start with upper case letter)"
    symbol "{"
    symbol "table" <?> "table (which table stores "<>T.unpack entityName<>"s?)"
    entityTable <- pQuotedLiteral False <?> "table name"
    entityColumns <- pColumnsList
    symbol "}"
    return $ Entity {..}

pColumnsList :: Parser [(Text, Text)]
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
            return (colName, colType)

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

data Assoc = Assoc
    { assocHeader :: AssocHeader
    , assocDefinition :: Predicate
    } deriving (Eq, Show, Ord)

pAssoc :: Parser Assoc
pAssoc = lexeme $ do
    assocHeader <- pAssocHeader
    symbol "if"
    assocDefinition <- pPredicate
    return Assoc {..}

data AssocHeader = AHPermission Permission | AHDef Definition
    deriving (Eq, Show, Ord)

pAssocHeader :: Parser AssocHeader
pAssocHeader = lexeme $
    (AHPermission <$> pPermission) <|> (AHDef <$> pDefinition)

data Permission = Permission
    { permissionType :: PermissionType
    , permissionActor :: TypedVar
    , permissionResource :: TypedVar
    } deriving (Eq, Show, Ord)

pPermission :: Parser Permission
pPermission = lexeme $ do
    permissionType <- pPermissionType
    symbol "("
    permissionActor <- pTypedVar
    symbol ","
    permissionResource <- pTypedVar
    symbol ")"
    return Permission {..}

data PermissionType = PCanSelect | PCanInsert | PCanUpdate | PCanDelete
    deriving (Eq, Show, Ord)

pPermissionType :: Parser PermissionType
pPermissionType = lexeme $ choice
    [ PCanSelect <$ pKeyword "can_select"
    , PCanInsert <$ pKeyword "can_insert"
    , PCanUpdate <$ pKeyword "can_update"
    , PCanDelete <$ pKeyword "can_delete"
    ]

data TypedVar = TypedVar
    { typedVarName :: Text
    , typedVarType :: Text
    } deriving (Eq, Show, Ord)

pTypedVar :: Parser TypedVar
pTypedVar = lexeme $ do
    typedVarName <- pLowerCasedWord
    symbol ":"
    typedVarType <- pTitleCasedWord <?> "type (an actor or resource)"
    return TypedVar{..}

data Definition = Definition
    { defName :: Text
    , defArgs :: [TypedVar]
    } deriving (Eq, Show, Ord)

pDefinition :: Parser Definition
pDefinition = lexeme $ do
    defName <- pLowerCasedWord
    symbol "("
    defArgs <- pCommaSepList pTypedVar
    symbol ")"
    return Definition {..}

data Predicate
    = PredCall Text [Value]
    | PAlways
    | PAnd Predicate Predicate
    | POr Predicate Predicate
    | PEquals Value Value
    | PGreaterT Value Value
    | PLessT Value Value
    deriving (Eq, Show, Ord)

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


data Value
    = VVar Text -- name of variable
    | VVarField Value Text -- object variable and field
    -- literals
    | VLitInt Int
    | VLitBool Bool
    | VLitString Text
    deriving (Eq, Show, Ord)

pValue :: Parser Value
pValue = lexeme $ choice
    [ VLitInt <$> L.signed empty L.decimal
    , VLitString <$> pQuotedLiteral True
    , VLitBool True <$ string "true"
    , VLitBool False <$ string "false"
    , VVar <$> pLowerCasedWord
    ] >>= pValueField

pValueField :: Value -> Parser Value
pValueField val = do
    field <- optional $ do
        char '.'
        pLowerCasedWord
    case field of
        Nothing -> return val
        Just fieldName -> pValueField $ VVarField val fieldName


-- esperado: (VVF (VVF (VV abc) def) ghi)
-- abc.def.ghi
