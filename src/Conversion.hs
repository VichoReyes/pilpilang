{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conversion where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Control.Monad.Reader (Reader, runReader, MonadReader (ask))
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State (MonadState, StateT (runStateT), execState)
import Control.Monad (forM, forM_)
import System.Random
import Types (TypedAST, ValidType (..), NonPrimitive, getNonPrimitive, tShow, typeOf, AssocKey (AssocKey, assocName))
import Common
import Lens.Micro.Platform
import Data.Word (Word8, Word32)
import Data.Either (isLeft)

{-

Primero se debe analizar el header para generar el estado inicial.
El estado debería tener:

- Conversiones de nicks de tablas: Map Value Text
- Asociaciones de nicks de tablas con las mismas tablas: [(Text, Text)]
Los 2 previos no son necesariamente del mismo tamaño, ya que puede haber tablas sin nicks
- Condiciones de JOINs: [Text]
- El StdGen para los números random
- La función que recopila todo y lo renderiza finalmente: MyState -> Text

-}

type TableNick = Text

type ConvertedPredicate = Text

data ConversorState = ConversorState
    { _valueNicks :: Map (GValue ValidType) TableNick
    , _tableNicks :: [(Text, TableNick)]
    , _joinConds :: [Text]
    , _randomGen :: StdGen
    , _renderFn :: ConversorState -> ConvertedPredicate -> Text
    }

emptyState :: ConversorState
emptyState = ConversorState
    { _valueNicks = M.empty
    , _tableNicks = []
    , _joinConds = []
    , _randomGen = mkStdGen 1
    , _renderFn = error "called renderFn before defining it"
    }

makeLenses ''ConversorState

type Conversor = StateT ConversorState (Reader TypedAST)

convertAll :: TypedAST -> [Text]
convertAll ast = map snd . filter (isLeft . assocName . fst) . M.toList . fmap (renderAssoc ast) $ ast

renderAssoc :: TypedAST -> GAssoc NonPrimitive ValidType -> Text
renderAssoc ast assoc =
    let initialState = mkInitialState (assoc^.assocHeader)
        (p, finalState) = flip runReader ast $ runStateT (renderPred (assoc^.assocDefinition)) initialState
    in  (finalState ^. renderFn) finalState p

mkInitialState :: GAssocHeader NonPrimitive -> ConversorState

mkInitialState (Left perm) = execState bootstrap emptyState
    where
        bootstrap = do
            assign valueNicks (M.fromList headerVars)
            assign tableNicks [(perm^.permissionActor._2.getNonPrimitive.entityTable, "actorNick")]
            forM_ (perm^.permissionExtraArgs) $ \(var, np) -> do
                let val = VVar var (TEntity np)
                expandScope val
            assign renderFn (permRender perm)

        headerVars =
            [
                (VVar (perm^.permissionActor._1) (perm^.permissionActor._2.to TEntity),
                "actorNick")
            ,
                (VVar (perm^.permissionResource._1) (perm^.permissionResource._2.to TEntity),
                -- Can't use a nick for the resource name
                perm^.permissionResource._2.getNonPrimitive.entityTable)
            ]

mkInitialState (Right def) = execState bootstrap emptyState
    where
        bootstrap = do
            forM_ (def^.defExtraArgs) $ \(var, np) -> do
                let val = VVar var (TEntity np)
                expandScope val
            keysSelected <- forM (def^.defArgs) $ \(var, np) -> do
                let val = VVar var (TEntity np)
                nick <- expandScope val
                return $ renderKeys nick np
            let defRender s p = "SELECT "
                    <> T.intercalate ", " keysSelected
                    <> " FROM "
                    <> renderTableNicks s
                    <> " WHERE "
                    <> T.intercalate " AND " (p : (s^.joinConds))
            assign renderFn defRender

renderTableNicks :: ConversorState -> Text
renderTableNicks state = T.intercalate ", " (state^..tableNicks.each.to (\(a, b) -> a<>" "<>b))

permRender :: GPermission NonPrimitive -> ConversorState -> ConvertedPredicate -> Text
permRender perm cs p = do
    let policyId = fst $ random (cs^.randomGen) :: Word32
    "create function pilpil"<>tShow policyId
        <>" (varchar(30), "<>perm^.permissionResource._2.getNonPrimitive.entityTable<>" "
        <>perm^.permissionResource._2.getNonPrimitive.entityTable<>")"
        <>" returns boolean as $$"
        <>" select ($1 IN ("
        <>" SELECT "<>T.intercalate "," (map ("actorNick."<>) (perm^.permissionActor._2.getNonPrimitive.entityKeys))
        <>" FROM "<>renderTableNicks cs
        <>" WHERE "<>T.intercalate " AND " (p : (cs^.joinConds))
        <>"));"
        <>"$$ language sql security definer;"
        <>" CREATE POLICY policy"<>tShow policyId
        <>" ON "<>perm^.permissionResource._2.getNonPrimitive.entityTable
        <>" FOR "<>renderPermType (perm^.permissionType)
        <>"(pilpil"<>tShow policyId<>"(current_user :: text, "
        <>perm^.permissionResource._2.getNonPrimitive.entityTable
        <>"));"

renderPermType :: PermissionType -> Text
renderPermType PCanDelete = "DELETE USING"
renderPermType PCanInsert = "INSERT WITH CHECK"
renderPermType PCanSelect = "SELECT USING"
renderPermType PCanUpdate = "UPDATE USING"
renderPermType PCanAnything = "ALL USING"

renderKeys :: Text -> NonPrimitive -> Text
renderKeys nick np = T.intercalate ", " $ map (\pk -> nick<>"."<>pk) (np^.getNonPrimitive.entityKeys)

renderPred :: GPredicate ValidType -> Conversor ConvertedPredicate
renderPred predicate = do
    case predicate of
        PAlways -> return "(1)"
        PAnd p1 p2 -> (\s1 s2 -> s1<>" AND "<>s2) <$> renderPred p1 <*> renderPred p2
        POr p1 p2 -> (\s1 s2 -> s1<>" OR "<>s2) <$> renderPred p1 <*> renderPred p2
        PEquals v1 v2 -> renderCmp " = " v1 v2
        PGreaterT v1 v2 -> renderCmp " > " v1 v2
        PLessT v1 v2 -> renderCmp " < " v1 v2
        PredCall txt vvs -> do
            let key = AssocKey (Right txt) (map typeOf vvs)
            predDef <- fromJust <$> view (at key)
            ast <- ask
            let subquery = renderAssoc ast predDef
            renderedVals <- mapM renderVal vvs
            return $ "("<>T.intercalate ", " renderedVals<>") IN ("<>subquery<>")"

        where
            renderCmp cmp v1 v2 = do
                s1 <- renderVal v1
                s2 <- renderVal v2
                return $ s1 <> cmp <> s2


-- render a value. There are different cases:
-- literal -> render as literal
-- variable -> look it up in scope, render. If it's not there, it's a programming failure.
-- value.field -> opens up two more cases
--    type of field is primitive -> "${render value}.field"
--    type of field is not primitive -> either
--        1. it's already in scope -> return it
--        2. expand the scope, create a condition, return the new nickname
renderVal :: GValue ValidType -> Conversor Text
renderVal (VLiteral l) = return $ renderLit l
renderVal val@(VVar _ (TEntity np)) = do
    nick <- renderValRec val
    return $ renderKeys nick np
renderVal val@(VVar _ (TPrimitive _)) = error $ "can't render "<>show val<>" yet, wait for an update"
renderVal (VVarField obj f (TPrimitive _)) = do
    t <- renderValRec obj
    return $ t <> "."<> f
renderVal val@(VVarField _ _ (TEntity np)) = do
    nick <- renderValRec val
    return $ renderKeys nick np

renderValRec :: (MonadState ConversorState m) => GValue ValidType -> m Text
renderValRec val@(VVar _ _) = fromJust <$> use (valueNicks . at val)
renderValRec val@(VVarField _ _ (TEntity _)) = do
    n <- use (valueNicks . at val)
    case n of
        Just nick -> return nick
        Nothing -> expandScope val
renderValRec _ = error "not implemented and illegal state"

renderLit :: Literal -> Text
renderLit (LitBool b) = tShow b
renderLit (LitString s) = "'"<>s<>"'"
renderLit (LitInt i) = tShow i

-- add value to the valueNicks, turning the value into a unique nickname combination
-- this combination will also be added to tableNicks
-- and a corresponding joinCond will be generated
expandScope :: (MonadState ConversorState m) => GValue ValidType -> m TableNick
expandScope val@(VVar _ (TEntity np)) = do
    let table = np ^. getNonPrimitive . entityTable
    nick <- genNick table
    assign (valueNicks . at val) (Just nick)
    modifying tableNicks (++ [(table, nick)])
    return nick
expandScope val@(VVarField obj f (TEntity np)) = do
    let table = np ^. getNonPrimitive . entityTable
    nick <- genNick table
    assign (valueNicks . at val) (Just nick)
    modifying tableNicks (++ [(table, nick)])
    let valPKs = renderKeys nick np
    let TEntity parent = typeOf obj
    let Just (Left (_, fKeys)) = parent^.getNonPrimitive.entityColumns.at f
    renderedParent <- renderValRec obj
    let objFKs = T.intercalate "," (((renderedParent<>".")<>) <$> fKeys)
    let newCond = "("<>valPKs<>") = ("<>objFKs<>")"
    modifying joinConds (++[newCond])
    return nick
expandScope _ = error "expandScope with forbidden values"

genNick' :: (RandomGen a) => a -> Text -> (TableNick, a)
genNick' r prefix = let (postfix, r') = random r
                        postfix :: Word8
                     in (prefix<>tShow postfix, r')

genNick :: (MonadState ConversorState m) => Text -> m TableNick
genNick prefix = runUntilUnique $ do
    r <- use randomGen
    let (nick, r') = genNick' r prefix
    assign randomGen r'
    return nick
        where
            runUntilUnique c = do
                v <- c
                allNicks <- use valueNicks <&> M.toList <&> map snd
                if v `elem` allNicks
                    then genNick prefix
                    else return v
