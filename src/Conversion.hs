{-# LANGUAGE OverloadedStrings #-}

module Conversion where

import Syntax
import Data.Maybe (mapMaybe, fromJust)
import Data.Text (Text)
import Control.Monad.Reader (Reader)
import Control.Monad.RWS (asks)
import qualified Data.Text as T
import Types (TypeInfo (entities), ValidType (..), ValidVal (ValidVal, vvContents), tShow)
import Data.Map ((!))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable (find)
import Control.Applicative ((<|>))

data ConverterInfo = ConverterInfo
    { ciAST :: AST
    , ciTypeInfo :: TypeInfo
    , ciValidPredicates :: [Predicate ValidVal]
    } deriving (Eq, Show)

type Conversor = Reader ConverterInfo

renderAST :: Conversor Text
renderAST = do
    ast <- asks ciAST
    let permissions = mapMaybe fromPermission $ astAssociations ast
    T.intercalate "\n" <$> traverse renderPermission permissions

renderPermission :: (Permission, Predicate ValidVal) -> Conversor Text
renderPermission (Permission perm act res, pred) = do
    -- let resType = asks fst
    actType <- asks ((!typedVarType act) . entities . ciTypeInfo)
    let TActor actEntity = actType
    resType <- asks ((!typedVarType res) . entities . ciTypeInfo)
    let TResource resEntity = resType
    let context = [(resVal resEntity, entityTable resEntity), (actVal actEntity, entityTable actEntity)]
    sqlPred <- renderPred context pred
    return $ renderPermType perm actEntity resEntity sqlPred
        where resVal ent = ValidVal (VVar $ typedVarName res) (TResource ent :| [])
              actVal ent = ValidVal (VVar $ typedVarName act) (TActor ent :| [])

renderPermType :: PermissionType -- ^ select, insert, update, etc
  -> GEntity ActorMarker (Text, ValidType) -- ^ actor entity, with typed columns 
  -> GEntity ResourceMarker (Text, ValidType) -- ^ resource entity, same
  -> Text -- ^ the predicate, already translated to SQL
  -> Text -- final policy
renderPermType pType act res sqlPred =
    "CREATE POLICY todo_name ON " <> res' <> " FOR " <> operation
        <> " " <> conditionOpener <> " (" <> actID act <> " IN ("
        <> sqlPred <> "));"
            where
                res' = entityTable res
                operation = case pType of
                  PCanSelect -> "SELECT"
                  PCanInsert -> "INSERT"
                  PCanUpdate -> "UPDATE"
                  PCanDelete -> "DELETE"
                conditionOpener = case pType of
                    PCanInsert -> "WITH CHECK"
                    _ -> "USING"
                actID _ = "current_user"

renderPred:: [(ValidVal, Text)] -> Predicate ValidVal -> Conversor Text
renderPred scope pred = do
    case pred of
        PAlways -> return "(1)"
        PAnd p1 p2 -> (\s1 s2 -> s1<>" AND "<>s2) <$> renderPred scope p1 <*> renderPred scope p2
        POr p1 p2 -> (\s1 s2 -> s1<>" OR "<>s2) <$> renderPred scope p1 <*> renderPred scope p2
        PEquals v1 v2 -> return $ renderCmp "=" v1 v2
        PGreaterT v1 v2 -> return $ renderCmp ">" v1 v2
        PLessT v1 v2 -> return $ renderCmp "<" v1 v2
        PredCall txt vvs -> do
            let start = "ROW("<>T.intercalate "," (map renderVal vvs)<>") IN ("
            assocs <- asks (astAssociations . ciAST)
            let newAssoc = fromJust $ find (findDef txt . assocHeader) assocs
            asdf <- renderAssoc newAssoc
            return $ start<>asdf<>")"

        where
            renderCmp cmp v1 v2 = do
                let s1 = renderVal v1
                    s2 = renderVal v2
                 in s1 <> cmp <> s2
            renderVal val = fromJust $ lookup val scope <|> renderLit val

renderAssoc :: Assoc -> Conversor Text
renderAssoc Assoc {assocHeader=ah, assocDefinition=p} = do
    (joinTables, conditions, scope) <- createScope assoc
    renderPred scope p

renderLit :: ValidVal -> Maybe Text
renderLit ValidVal {vvContents = VLitBool b} = Just $ tShow b
renderLit ValidVal {vvContents = VLitString b} = Just $ "'"<>b<>"'" -- TODO escape
renderLit ValidVal {vvContents = VLitInt b} = Just $ tShow b
renderLit _ = Nothing

findDef :: Text -> AssocHeader -> Bool
findDef txt header = case header of
  AHDef Definition {defName=name} -> name == txt
  _ -> False

fromPermission :: Assoc -> Maybe (Permission, Predicate ValidVal)
fromPermission assoc = case assocHeader assoc of
    AHPermission per -> Just (per, undefined assocDefinition assoc)
    AHDef _ -> Nothing

