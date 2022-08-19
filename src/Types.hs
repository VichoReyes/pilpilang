{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types where

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
    , actorColumns :: [Text]
    } deriving (Eq, Show, Ord)

stringLiteral :: Parser Text
stringLiteral = char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

pActor :: Parser Actor
pActor = do
    pKeyword "actor"
    actorName <- lexeme 
        (T.pack <$> ((:) <$> upperChar <*> many alphaNumChar))
        <?> "actor name (should start with upper case letter)"
    symbol' "{"
    symbol' "table"
    actorTable <- pQuotedLiteral
    symbol' "columns"
    symbol' "["
    actorColumns <- pLiteralsList
    symbol' "]"
    symbol' "}"
    return Actor{..}

pLiteralsList :: Parser [Text]
pLiteralsList = pLiteralsList2 <|> pure []
    where
        pLiteralsList2 = do
            elem <- pQuotedLiteral
            canContinue <- True <$ symbol' "," <|> pure False
            if canContinue
                then (elem : ) <$> pLiteralsList
                else return [elem]

pQuotedLiteral :: Parser Text
pQuotedLiteral = lexeme $ do
    char '"'
    literal <- manyTill L.charLiteral (char '"')
    return (T.pack literal)

