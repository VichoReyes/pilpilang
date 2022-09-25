{-# LANGUAGE OverloadedStrings #-}
module DBSchema where

import Database.PostgreSQL.Simple (connect, defaultConnectInfo, ConnectInfo (..), query_, FromRow, close, Only (Only))
import Data.Text (Text)
import qualified Data.Text as T
import Types (Type, ColumnTypeProvider (fillTypes), TypeError (TypeError))
import Data.Map (Map)
import qualified Data.Map as M
import Syntax (GEntity (entityTable, Entity, entityColumns))
import Data.Maybe (isJust, catMaybes)


{-
Relevant SELECTs we need to do:

SELECT
    table_schema,
    table_name,
    column_name,
    data_type
FROM
information_schema.columns


-- no tengo claro qué hacer para encontrar
las foreign keys, pero probablemente las vistas útiles sean (del schema information_schema):
- referential_constraints
- constraint_table_usage
-}

-- getStuff :: IO [(Text, Text, Text, Text)]
getStuff = do
    conn <- connect defaultConnectInfo {connectPassword ="password", connectDatabase = "dvdrental"}
    results <- query_ conn "SELECT table_schema, table_name, column_name, data_type FROM information_schema.columns"
    close conn
    let reshaped = catMaybes . map convertType' $ results
    return reshaped
        where 
            convertType' (a, b, c, d) = do
                t <- convertType d
                return ((a, b), (c, t))
   

newtype DBTypeProvider = DBTypeProvider {typeMap :: Map (Text, Text) [(Text, Maybe Type)]}

instance ColumnTypeProvider DBTypeProvider where
    fillTypes = dbFillTypes

dbFillTypes :: DBTypeProvider -> GEntity klass Text -> Either TypeError (GEntity klass (Text, Text))
dbFillTypes types entity = do
    let table = entityTable entity
    let splitTable = T.split (== '.') table
    let schemaTable = if length splitTable == 2
        then (splitTable !! 0, splitTable !! 1)
        else ("public", table)
    columnList <- maybe (Left . TypeError $ "table "<>T.pack (show schemaTable)<>" not found")
        Right (M.lookup schemaTable (typeMap types))
    return entity {entityColumns = columnList}

{-
entre otros tipos que podemos encontrar:

números:
    enteros:
        smallint
        integer
        bigint   
        smallserial
        serial
        bigserial

    no enteros:
        numeric
        decimal
        real
        double precision

strings:
    char
    character
    varchar
    character varying
    text

boolean

Y hay varios otros como timestamps, arrays o tipos custom.
-}

convertType :: Text -> Maybe Type
convertType dbType
    | dbType `elem` ["char", "character", "varchar", "character varying", "text"] = Just "String"
    | dbType `elem` ["smallint", "integer", "bigint", "smallserial", "serial", "bigserial"] = Just "Int"
    | dbType `elem` ["numeric", "decimal", "real", "double precision"] = Just "Int" -- TODO: make "float" type
    | dbType == "boolean" = Just "Bool"
    | otherwise = Nothing

