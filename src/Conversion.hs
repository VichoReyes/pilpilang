{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conversion where

import Data.Maybe (fromJust)
import Data.Text (Text)
import Control.Monad.Reader (Reader, runReader, MonadReader (ask))
import Control.Monad.RWS (asks)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Control.Monad.State (get, MonadState (put), StateT (runStateT), evalStateT, lift, evalState, execState)
import Control.Monad (forM, forM_)
import System.Random
import Types (TypedAST, ValidType (..), Primitive, NonPrimitive, getNonPrimitive, tShow, typeOf, AssocKey (AssocKey))
import Common
import Lens.Micro.Platform
import Common (GValue(VVar))
import Data.Int (Int8)
import Control.Applicative ((<|>))
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
    , _tableNicks :: [(Text, TableNick)] -- table, nickname
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
    , _renderFn = undefined -- ConversorState -> ConvertedPredicate -> Text
    }

makeLenses ''ConversorState

type Conversor = StateT ConversorState (Reader TypedAST)

{-
data Query = Query
    { qSelect :: [(Text, Text)] -- SELECT a.b, c.d 
    , qFrom :: [(Text, (Char, Int))]     -- FROM arts a1, cuisines c1
    , qWhere :: [Text]
    } deriving (Show, Eq, Ord)

qRender :: Query -> Text
qRender Query {qSelect = select', qFrom = from', qWhere = where'}
    = "SELECT "<>T.intercalate ", " (map (\(a, b) -> a<>"."<>b) select')
    <> " FROM "<>T.intercalate ", " (map (\(a, (c, i)) -> a<>" "<>T.pack (c : show i)) from')
    <> " WHERE "<>T.intercalate " AND " where'
-}

convertAll :: TypedAST -> [Text]
convertAll ast = map snd . M.toList . fmap (renderAssoc ast) $ ast

renderAssoc :: TypedAST -> GAssoc NonPrimitive ValidType -> Text
renderAssoc ast assoc =
    let initialState = mkInitialState (assoc^.assocHeader)
        (p, finalState) = flip runReader ast $ runStateT (renderPred (assoc^.assocDefinition)) initialState
    in  (finalState ^. renderFn) finalState p

mkInitialState :: GAssocHeader NonPrimitive -> ConversorState

mkInitialState (Left perm) =
    emptyState
        & valueNicks .~ M.fromList headerVars
        & renderFn .~ flip const
    where
        headerVars =
            [
                (VVar (perm^.permissionActor._1) (perm^.permissionActor._2.to TEntity),
                "actorNick")
            , 
                (VVar (perm^.permissionResource._1) (perm^.permissionResource._2.to TEntity),
                -- Can't use a nick for the table name
                perm^.permissionResource._2.getNonPrimitive.entityTable)
            ]

mkInitialState (Right def) = execState bootstrap emptyState
    where
        bootstrap = do
            keysSelected <- forM (def^.defArgs) $ \(var, np) -> do
                let val = VVar var (TEntity np)
                nick <- expandScope val
                return $ T.intercalate ", " $ map (\pk -> nick<>"."<>pk) (np^.getNonPrimitive.entityKeys)
            let defRender s p = "SELECT " 
                    <> T.intercalate ", " keysSelected 
                    <> " FROM "
                    <> T.intercalate ", " (s^..tableNicks.each.to (\(a, b) -> a<>" "<>b))
                    <> " WHERE "
                    <> T.intercalate " AND " (p : (s^.joinConds))
            assign renderFn defRender

{-
renderAssoc2 header varNames predicate = do
    select' <- mkScope header
    (joinTables, conditions) <- removeIndirections predicate
    predWhere <- renderPred predicate
    return Query
        { qFrom = joinTables ++ zip varNames select' -- TODO not varNames but table names
        , qSelect = map (\(c, i) -> (T.pack $ c : show i, "some_pk")) select'
        , qWhere = predWhere : conditions
    }
        where
            mkScope (HDefinition _ types) = expandScope $ zipWith makeVal varNames types
            mkScope (HPermission _ act res) = expandScope $ zipWith makeVal varNames [TActor act, TResource res]
            makeVal name typ = ValidVal (VVar name) (NE.fromList [typ])
-}

renderPred :: GPredicate ValidType -> Conversor ConvertedPredicate
renderPred predicate = do
    case predicate of
        PAlways -> return "(1)"
        PAnd p1 p2 -> (\s1 s2 -> s1<>" AND "<>s2) <$> renderPred p1 <*> renderPred p2
        POr p1 p2 -> (\s1 s2 -> s1<>" OR "<>s2) <$> renderPred p1 <*> renderPred p2
        PEquals v1 v2 -> renderCmp "=" v1 v2
        PGreaterT v1 v2 -> renderCmp ">" v1 v2
        PLessT v1 v2 -> renderCmp "<" v1 v2
        PredCall txt vvs -> do
            let key = AssocKey (Right txt) (map typeOf vvs)
            predDef <- fromJust <$> view (at key)
            ast <- ask
            let subquery = renderAssoc ast predDef
            renderedVals <- mapM renderVal vvs
            return $ "("<>T.intercalate "," renderedVals<>") IN ("<>subquery<>")"

        where
            renderCmp cmp v1 v2 = do
                s1 <- renderVal v1
                s2 <- renderVal v2
                return $ s1 <> cmp <> s2


-- render a value. There are different cases:
-- literal -> render as literal
-- variable -> look it up in scope, render
-- value.field -> opens up two more cases
--    type of field is primitive -> "${render value}.field"
--    type of field is not primitive -> look it up in scope, render
-- non primitives should have been put in scope by previous phase
renderVal :: GValue ValidType -> Conversor Text
renderVal (VLiteral l) = return $ renderLit l
renderVal val = do
    n <- use (valueNicks . at val)
    case (n, typeOf val) of
        (Just nick, _) -> return nick
        (Nothing, TEntity _) -> expandScope val
        (Nothing, TPrimitive _) -> do
            obj <- renderVal (val ^?! valObject)
            return $ obj<>"."<>(val ^?! valField)

{-
renderVal val@ValidVal {vvContents = VVarField v f} = do
    scope <- get
    case lookup val scope of
        Just x -> replace x
        Nothing -> do
            tableThing <- renderVal (ValidVal v (NE.fromList (NE.tail (vvType val))))
            return $ tableThing <> "." <> f
renderVal val = do
    scope <- get
    case lookup val scope of
        Just x -> replace x
        Nothing -> error $ "renderVal: scope = "<>show scope<>" and val = "<>show val
-}


renderLit :: Literal -> Text
renderLit (LitBool b) = tShow b
renderLit (LitString s) = "'"<>s<>"'"
renderLit (LitInt i) = tShow i

-- add value to the valueNicks, turning the value into a unique nickname combination
-- this combination will also be added to tableNicks
expandScope :: (MonadState ConversorState m) => GValue ValidType -> m TableNick
expandScope val@(VVar _ (TEntity np)) = do
    let table = np ^. getNonPrimitive . entityTable
    nick <- genNick table
    assign (valueNicks . at val) (Just nick)
    modifying tableNicks (++ [(table, nick)])
    return nick
expandScope val@(VVarField _ _ (TEntity np)) = do
    let table = np ^. getNonPrimitive . entityTable
    nick <- genNick table
    assign (valueNicks . at val) (Just nick)
    modifying tableNicks (++ [(table, nick)])
    -- TODO: add condition and key
    return nick
expandScope _ = undefined

{-
removeIndirections :: GPredicate ValidType -> Conversor ([(Text, (Char, Int))], [Text])
removeIndirections predicate = unzip . concat <$> traverse extractInfo values
    where
        values :: [ValidVal]
        values = getValues predicate
        getValues PAlways = []
        getValues (PredCall _ args) = args
        getValues (PAnd p1 p2) = getValues p1 ++ getValues p2
        getValues (POr p1 p2) = getValues p1 ++ getValues p2
        getValues (PEquals v1 v2) = [v1, v2]
        getValues (PGreaterT v1 v2) = [v1, v2]
        getValues (PLessT v1 v2) = [v1, v2]

        extractInfo :: ValidVal -> Conversor [((Text, (Char, Int)), Text)]
        extractInfo v = case vvContents v of
            VLitInt _ -> return []
            VLitBool _ -> return []
            VLitString _ -> return []
            VVar _ -> return []
            VVarField inner txt -> do
                let innerVal = ValidVal inner (NE.fromList $ NE.tail $ vvType v)
                innerResults <- extractInfo innerVal
                if isPrimitive (NE.head (vvType v))
                    then return innerResults
                    else do
                        tableNickname <- head <$> expandScope [v]
                        let tableFullName = case NE.head (vvType v) of
                                TActor ent -> entityTable ent
                                TResource ent -> entityTable ent
                                _ -> error "illegal state"
                        let condition = "("<>txt<>" = some other key)"
                        -- return both
                        return $ ((tableFullName, tableNickname), condition) : innerResults
-}

genNick' :: (RandomGen a) => a -> Text -> (TableNick, a)
genNick' r prefix = let (postfix, r') = random r
                        postfix :: Int8
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
