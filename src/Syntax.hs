{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Syntax where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void Text

-- Skipper of whitespace. Doesn't accept comments
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

-- Use this function to skip whitespace after a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

data Actor = Actor
    { actorName :: Text
    , actorTable :: Text
    , actorColumns :: Maybe [Text]
    } deriving (Eq, Show, Ord)

data Resource = Resource
    { resourceName :: Text
    , resourceTable :: Text
    , resourceColumns :: Maybe [Text]
    } deriving (Eq, Show, Ord)

stringLiteral :: Parser Text
stringLiteral = char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

pActor :: Parser Actor
pActor = do
    pKeyword "actor"
    actorName <- pTitleCasedWord
        <?> "actor name (should start with upper case letter)"
    symbol "{"
    symbol "table" <?> "table (which table stores "<>T.unpack actorName<>"s?)"
    actorTable <- pQuotedLiteral False <?> "table name"
    actorColumns <- pColumnsList
    symbol "}"
    return Actor{..}

-- yes this is repetitive, but it's not so bad I think
pResource :: Parser Resource
pResource = do
    pKeyword "resource"
    resourceName <- pTitleCasedWord
        <?> "resource name (should start with upper case letter)"
    symbol "{"
    symbol "table" <?> "table (which table stores "<>T.unpack resourceName<>"s?)"
    resourceTable <- pQuotedLiteral False <?> "table name"
    resourceColumns <- pColumnsList
    symbol "}"
    return Resource{..}

pColumnsList :: Parser (Maybe [Text])
pColumnsList = optional $ do
    symbol "columns"
    symbol "["
    columns <- pCommaSepList (pQuotedLiteral True)
    symbol "]"
    return columns

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
            elem <- pElem
            canContinue <- True <$ symbol "," <|> pure False
            if canContinue
                then (elem : ) <$> pCommaSepList pElem
                else return [elem]

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
    , permissionActor :: OptTypeVar
    , permissionResource :: OptTypeVar
    } deriving (Eq, Show, Ord)

pPermission :: Parser Permission
pPermission = lexeme $ do
    permissionType <- pPermissionType
    symbol "("
    permissionActor <- pOptTypeVar
    symbol ","
    permissionResource <- pOptTypeVar
    symbol ")"
    return Permission {..}

data PermissionType = PCanRead | PCanWrite | PCanDelete
    deriving (Eq, Show, Ord)

pPermissionType :: Parser PermissionType
pPermissionType = lexeme $ choice
    [ PCanRead <$ pKeyword "can_read"
    , PCanWrite <$ pKeyword "can_write"
    , PCanDelete <$ pKeyword "can_delete"
    ]

data OptTypeVar = OptTypeVar
    { otvarName :: Text
    , otvarType :: Maybe Text
    } deriving (Eq, Show, Ord)

pOptTypeVar :: Parser OptTypeVar
pOptTypeVar = lexeme $ do
    otvarName <- pLowerCasedWord
    otvarType <- optional $ do
        symbol ":"
        pTitleCasedWord <?> "type (an actor or resource)"
    return OptTypeVar{..}

data Definition = Definition
    { defName :: Text
    , defArgs :: [OptTypeVar]
    } deriving (Eq, Show, Ord)

pDefinition :: Parser Definition
pDefinition = lexeme $ do
    defName <- pLowerCasedWord
    symbol "("
    defArgs <- pCommaSepList pOptTypeVar
    symbol ")"
    return Definition {..}

data Predicate
    = PCall PredCall
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
            [ PCall <$> try pPredCall
            -- TODO: Do this with makeExprParser too
            , PEquals <$> try (pValue <* symbol "=") <*> pValue
            , PGreaterT <$> try (pValue <* symbol ">") <*> pValue
            , PLessT <$> try (pValue <* symbol "<") <*> pValue
            , between (symbol "(") (symbol ")") pPredicate
            ]
        operatorTable = [
            [ InfixL (PAnd <$ symbol "&&")
            , InfixR (POr <$ symbol "||")
            ]]

data PredCall = PredCall
    { predCallName :: Text
    , predCallArgs :: [Text]
    } deriving (Eq, Show, Ord)

pPredCall :: Parser PredCall
pPredCall = do
    predCallName <- pLowerCasedWord
    symbol "("
    predCallArgs <- pCommaSepList pLowerCasedWord
    symbol ")"
    return PredCall {..}


data Value
    = VVar Text -- name of variable
    | VVarField Text Text -- object variable and field
    | VLiteral Int -- for now the only literals are integers
    deriving (Eq, Show, Ord)

pValue :: Parser Value
pValue = lexeme $ choice
    [ VLiteral <$> L.signed empty L.decimal
    , try pVarField
    , VVar <$> pLowerCasedWord
    ]
        where 
            pVarField = do
                var <- pLowerCasedWord
                char '.'
                field <- pLowerCasedWord
                return (VVarField var field)
