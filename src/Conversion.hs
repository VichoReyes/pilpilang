{-# LANGUAGE OverloadedStrings #-}

module Conversion where

import Syntax
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Control.Monad.Reader (Reader)
import Data.Foldable (fold)
import Control.Monad.RWS (asks)
import qualified Data.Text as T
import Types (TypeInfo (entities), ValidType (..), ValidVal, tShow)
import Data.Map ((!))

data ConverterInfo = ConverterInfo
    { ciAST :: AST
    , ciTypeInfo :: TypeInfo
    , ciScope :: [(Text, (Char, Int))]
    } deriving (Eq, Show)

type Conversor = Reader ConverterInfo

asdf :: Conversor Text
asdf = do
    ast <- asks ciAST
    let permissions = mapMaybe fromPermission $ astAssociations ast
    fold <$> traverse qwer permissions

qwer :: (Permission, Predicate ValidVal) -> Conversor Text
qwer (Permission perm act res, pred) = do
    -- let resType = asks fst
    actType <- asks ((!typedVarType act) . entities . ciTypeInfo)
    let TActor actEntity = actType
    resType <- asks ((!typedVarType res) . entities . ciTypeInfo)
    let TResource resEntity = resType
    (tables, allConditions) <- renderPred pred
    let allTables = T.intercalate ", " $ map renderTable tables
    return $ "CREATE POLICY todo_name ON "<>entityTable resEntity<>" t1 FOR "
        <>ptype2sql perm<>" (EXISTS (SELECT * FROM "<>allTables<>" WHERE "<>allConditions

renderPred :: Predicate ValidVal -> Conversor ([(Text, (Char, Int))], Text)
renderPred (PredCall predName vals) = undefined
renderPred (PAnd p1 p2) = do
    (tables1, conditions1) <- renderPred p1
    (tables2, conditions2) <- renderPred p2
    return (tables1 ++ tables2, "("<>conditions1<>" AND "<>conditions2<>")")
renderPred (POr p1 p2) = do
    (tables1, conditions1) <- renderPred p1
    (tables2, conditions2) <- renderPred p2
    return (tables1 ++ tables2, "("<>conditions1<>" OR "<>conditions2<>")")
renderPred PAlways = return ([], "true")
renderPred (PEquals v1 v2) = undefined
renderPred (PGreaterT v1 v2) = undefined
renderPred (PLessT v1 v2) = undefined

renderTable :: (Text, (Char, Int)) -> Text
renderTable (table, (initial, num)) = tShow table<>" "<>tShow initial<>tShow num

ptype2sql :: PermissionType -> Text
ptype2sql PCanDelete = "DELETE USING"
ptype2sql PCanInsert = "INSERT WITH CHECK"
ptype2sql PCanSelect = "SELECT USING"
ptype2sql PCanUpdate = "UPDATE USING" -- TODO add with check in this case too


fromPermission :: Assoc -> Maybe (Permission, Predicate ValidVal)
fromPermission assoc = case assocHeader assoc of
    AHPermission per -> undefined per -- Just (per, assocDefinition assoc)
    AHDef _ -> Nothing

