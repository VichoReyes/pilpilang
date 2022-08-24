{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Syntax where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Void (Void)
import Data.Functor (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void T.Text

-- Skipper of whitespace. Doesn't accept comments
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

-- Use this function to skip whitespace after a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

symbol' = void . symbol

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
    symbol' "{"
    symbol' "table" <?> "table (which table stores "<>T.unpack actorName<>"s?)"
    actorTable <- pQuotedLiteral False <?> "table name"
    actorColumns <- pColumnsList
    symbol' "}"
    return Actor{..}

-- yes this is repetitive, but it's not so bad I think
pResource :: Parser Resource
pResource = do
    pKeyword "resource"
    resourceName <- pTitleCasedWord
        <?> "resource name (should start with upper case letter)"
    symbol' "{"
    symbol' "table" <?> "table (which table stores "<>T.unpack resourceName<>"s?)"
    resourceTable <- pQuotedLiteral False <?> "table name"
    resourceColumns <- pColumnsList
    symbol' "}"
    return Resource{..}

pColumnsList :: Parser (Maybe [Text])
pColumnsList = optional $ do
    symbol' "columns"
    symbol' "["
    columns <- pLiteralsList
    symbol' "]"
    return columns

pTitleCasedWord :: Parser Text
pTitleCasedWord = lexeme $ do
    first <- upperChar
    rest <- many (alphaNumChar <|> char '_')
    return $ T.pack (first:rest)

pLiteralsList :: Parser [Text]
pLiteralsList = pLiteralsList2 <|> pure []
    where
        pLiteralsList2 = do
            elem <- pQuotedLiteral True
            canContinue <- True <$ symbol' "," <|> pure False
            if canContinue
                then (elem : ) <$> pLiteralsList
                else return [elem]

pQuotedLiteral :: Bool -> Parser Text
pQuotedLiteral allowEmpty = lexeme $ do
    char '"'
    prefix <- if allowEmpty
        then return ""
        else (:[]) <$> L.charLiteral
    literal <- manyTill L.charLiteral (char '"')
    return $ T.pack (prefix <> literal)

