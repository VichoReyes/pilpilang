{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Syntax
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State (StateT, MonadTrans (lift), execStateT)
import Control.Monad (forM_, when, forM)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask, local))
import qualified Data.Text as T
import Data.String (IsString)
import Lens.Micro.Platform
import Data.Either (fromRight)

tShow :: Show a => a -> Text
tShow = T.pack . show

{-
This should eventually connect to a database
class ColumnTypeProvider a where
    fillTypes :: a -> GEntity klass Text -> Either TypeError (GEntity klass (Text, Text))
-}

newtype TypeError = TypeError
    { getTypeError :: Text
    } deriving (Eq, Ord, Show, Semigroup, Monoid, IsString)

type Type = Text

newtype NonPrimitive = NonPrimitive
        { _getNonPrimitive :: GEntity NonPrimitive Primitive }
    deriving (Eq, Ord, Show)

data ValidType
    = TPrimitive
        { _typePrimitive :: Primitive }
    | TEntity
        { _typeNonPrimitive :: NonPrimitive }
    deriving (Eq, Ord, Show)

data Primitive
    = TInt
    | TBool
    | TString
    deriving (Eq, Ord, Show)

makeLenses ''ValidType
makeLenses ''NonPrimitive

isPrimitive :: ValidType -> Bool
isPrimitive (TPrimitive _) = True
isPrimitive (TEntity _) = False

data AssocKey = AssocKey
    { assocName :: Either PermissionType Text
    , assocTypes :: [ValidType]
    } deriving (Eq, Ord, Show)

data PartialInfo = PartialInfo
    { _piEntities :: Map Text Entity
    , _piFunctions :: Map Text [Type] -- types of the arguments
    } deriving (Eq, Ord, Show)

makeLenses ''PartialInfo

data TypeInfo = TypeInfo
    { _entities :: Map Type NonPrimitive -- actor/resource name -> Entity
    , _functions :: Map Text [ValidType] -- types of the arguments
    , _variables :: Map Text ValidType
    } deriving (Eq, Show)

instance Semigroup TypeInfo where
    ti1 <> ti2 = TypeInfo
        { _entities = _entities ti1 <> _entities ti2
        , _functions = _functions ti1 <> _functions ti2
        , _variables = _variables ti1 <> _variables ti2 }

instance Monoid TypeInfo where
    mempty = TypeInfo
        { _entities = M.empty
        , _functions = M.empty
        , _variables = M.empty }

makeLenses ''TypeInfo

type TypeGen a = StateT PartialInfo (Either TypeError) a

type TypeCheck a = ReaderT TypeInfo (Either TypeError) a

typeFail :: Text -> Either TypeError a
typeFail = Left . TypeError

type TypedAST = Map AssocKey (GAssoc NonPrimitive ValidType)

runTypeChecker :: AST -> Either TypeError TypedAST
runTypeChecker ast = do
    partialGlobals <- execStateT (mkGlobals ast) mempty
    globals <- materialize partialGlobals ast
    runReaderT (cAssocs (ast ^. astAssociations)) globals

materialize :: PartialInfo -> AST -> Either TypeError TypeInfo
materialize (PartialInfo ents funcs) ast = do
    let entNames = M.keys ents
    ents' <- mapM (genEnt ast) entNames
    let entPairs = zip entNames ents'
    funcs' <- traverse (mapM $ lookupArgType entPairs) funcs
    return mempty <&> entities .~ M.fromList entPairs
                  <&> functions .~ funcs'
        where
            lookupArgType entPairs typeName = case lookup typeName entPairs of
                Nothing -> Left . TypeError $ typeName<>" not found"
                Just b -> Right (TEntity b)

genEnt :: AST -> Text -> Either TypeError NonPrimitive
genEnt ast entName = do
    theType <- genType ast entName
    case theType of
        TPrimitive _ -> typeFail (entName <> " is a primitive but should not be")
        TEntity np -> return np

genType :: AST -> Text -> Either TypeError ValidType
genType _ "Int" = return (TPrimitive TInt)
genType _ "String" = return (TPrimitive TString)
genType _ "Bool" = return (TPrimitive TBool)
genType ast entName = TEntity . NonPrimitive <$> do
    case ast ^? astEntities . each . filtered (\a -> a ^. entityName == entName) of
        Nothing -> typeFail $ entName <>" not found"
        Just ent -> do
            return $ columnsMap colWithKeys colWithout ent
    where
        colWithout :: Text -> Primitive
        colWithout colType = case fromRight undefined $ genType ast colType of
            TPrimitive t -> t
            TEntity _ -> error "there should be keys"
        colWithKeys :: [Text] -> Text -> NonPrimitive
        colWithKeys keysList colType = case fromRight undefined $ genType ast colType of
            TPrimitive _ -> error "there should not be keys for a primitive"
            TEntity e -> if length keysList == length (e ^. getNonPrimitive . entityKeys)
                then e
                else error "wrong number of foreign keys"

addEntity :: Entity -> TypeGen ()
addEntity a = do
    existingEntity <- use (piEntities . at (a ^. entityName))
    case existingEntity of
        Just _ ->
            lift . typeFail $ "entity "<>a ^. entityName<>" defined twice"
        Nothing ->
            piEntities . at (a ^. entityName) .= Just a

mkGlobals :: AST -> TypeGen ()
mkGlobals ast = do
    mapM_ addEntity (ast ^. astEntities)
    forM_ (ast ^. astAssociations) $ \assoc ->
        case assoc ^. assocHeader of
            Right (GDefinition name args) -> addFunction name args
            _ -> return () -- skip permissions, they always have the same types

cTypeError :: Text -> TypeCheck a
cTypeError err = lift (Left (TypeError err))

cLookUp :: (TypeInfo -> Maybe a) -> Text -> TypeCheck a
cLookUp f err = do
    typeInfo <- ask
    case f typeInfo of
        Nothing -> cTypeError err
        Just a -> return a

-- 1. obtener variables, tipos del header
-- 2. meterlos en la llamada local de cPredicate
-- 3. Con el nombre y los tipos del header, hacer un AssocKey
-- 4. retornar (assockey, nuevaAssoc)
cAssocs :: [Assoc] -> TypeCheck TypedAST
cAssocs associations = M.fromList <$> do
    forM associations $ \assoc -> do
        let predicate = assoc ^. assocDefinition
        localVars <- mkLocalVars (assoc ^. assocHeader)
        p <- local (\ti -> ti {_variables = M.fromList (localVars <&> _2 %~ TEntity)}) (cPredicate predicate)
        let header = mkHeader localVars (assoc ^. assocHeader)
        let key = mkKey header
        return (key, assoc & assocDefinition .~ p & assocHeader .~ header)

mkKey :: GAssocHeader NonPrimitive -> AssocKey
mkKey (Right (GDefinition name vars)) = AssocKey {assocName=Right name, assocTypes=map (TEntity . snd) vars}
mkKey (Left (GPermission perm act res)) = AssocKey {assocName=Left perm, assocTypes=map (TEntity . snd) [act, res]}

mkHeader :: [(Text, NonPrimitive)] -> AssocHeader -> GAssocHeader NonPrimitive
mkHeader localVars (Right (GDefinition name _)) = Right $ GDefinition name localVars
mkHeader [act, res] (Left (GPermission perm _ _)) = Left $ GPermission perm act res
mkHeader _ _ = error "illegal state"

mkLocalVars :: AssocHeader -> TypeCheck [(Text, NonPrimitive)]
mkLocalVars (Right (GDefinition _ vars)) = do
    types <- mapM (getEnt . snd) vars
    return $ zip (map fst vars) types
    -- return . Right $ GDefinition name (zip (map fst vars) types)
mkLocalVars (Left (GPermission _ actorVar resourceVar)) = do
    types <- mapM (getEnt . snd) [actorVar, resourceVar]
    let (actorType, resourceType) = (head types, types!!1)
    when (actorType ^. getNonPrimitive . entityClass /= EActor) $
        lift . typeFail $ tShow actorType<>": when defining a permission, first argument should be an Actor"
    when (resourceType ^. getNonPrimitive . entityClass /= EResource) $
        lift . typeFail $ tShow resourceType<>": when defining a permission, second argument should be a Resource"
    return [(fst actorVar, actorType), (fst resourceVar, resourceType)]

getEnt :: Text -> TypeCheck NonPrimitive
getEnt typeName = do
    ent <- view (entities . at typeName)
    lift $ maybe (typeFail "ent not found") Right ent

type TypedVar = (Text, Text)

cPredicate :: GPredicate () -> TypeCheck (GPredicate ValidType)
cPredicate (PredCall predName args) = do
    expectedTypes <- cLookUp (^. functions . at predName) $
        predName<>" not defined"
    when (length expectedTypes /= length args) $
        cTypeError (predName<>" call with wrong number of arguments")
    args' <- mapM cValue args
    forM_ (zip expectedTypes (map typeOf args')) $ \(ex, act) ->
        when (ex /= act)
            (cTypeError $ "in call to "<>predName<>": expected "<>tShow ex<>", found "<>tShow act)
    return (PredCall predName args')
cPredicate PAlways = return PAlways
cPredicate (PAnd p1 p2) = PAnd <$> cPredicate p1 <*> cPredicate p2
cPredicate (POr p1 p2) = POr <$> cPredicate p1 <*> cPredicate p2
cPredicate (PEquals val1 val2) = do
    val1' <- cValue val1
    val2' <- cValue val2
    when (typeOf val1' /= typeOf val2') $ do
        cTypeError ("mismatched types in "<>tShow val1'<>" = "<>tShow val2'<>
            ": first is "<>tShow (typeOf val1')<>" and second is "<>tShow (typeOf val2'))
    return $ PEquals val1' val2'
cPredicate (PGreaterT val1 val2) = do
    val1' <- cValue val1
    when (typeOf val1' /= TPrimitive TInt) $
        cTypeError (tShow (typeOf val1')<>"doesn't support order comparison")
    val2' <- cValue val2
    when (typeOf val2' /= TPrimitive TInt) $
        cTypeError (tShow (typeOf val2')<>"doesn't support order comparison")
    return $ PGreaterT val1' val2'
-- reuse the last one
cPredicate (PLessT val1 val2) = cPredicate (PGreaterT val2 val1)

typeOf :: GValue ValidType -> ValidType
typeOf (VVar _ t) = t
typeOf (VVarField _ _ t) = t
typeOf (VLiteral l) = TPrimitive $ literalType l
    where
        literalType :: Literal -> Primitive
        literalType (LitBool _) = TBool
        literalType (LitInt _) = TInt
        literalType (LitString _) = TString

cValue :: Value -> TypeCheck (GValue ValidType)
cValue (VLiteral l) = return $ VLiteral l
cValue (VVar varName ()) = do
    varType <- view (variables . at varName)
    lift $ maybe (typeFail (varName<>" not found"))
        (Right . VVar varName) varType
cValue (VVarField obj field ()) = do
    typedObj <- cValue obj
    case typedObj ^? to typeOf . typeNonPrimitive . getNonPrimitive . entityColumns . at field . _Just of
        Nothing -> lift . typeFail $ tShow obj<>" has no field"<>field
        Just fieldColType -> do
            let fieldType = fieldColType
                    ^?! (_Left . _1 . to TEntity <> _Right . to TPrimitive)
            return $ VVarField typedObj field fieldType

-- propertyLookup :: Text -> NonPrimitive -> Maybe ValidType
-- propertyLookup k np = np ^. entityColumns . at k

addFunction :: Text -> [TypedVar] -> TypeGen ()
addFunction name args = do
    currentFunction <- use (piFunctions . at name)
    case currentFunction of
        Nothing -> piFunctions . at name .= Just (map snd args)
        Just _ -> lift . typeFail $  "function "<>name<>" defined twice"

instance Semigroup PartialInfo where
    ti1 <> ti2 = PartialInfo
        { _piEntities = _piEntities ti1 <> _piEntities ti2
        , _piFunctions = _piFunctions ti1 <> _piFunctions ti2 }

instance Monoid PartialInfo where
    mempty = PartialInfo
        { _piEntities = M.empty
        , _piFunctions = M.empty
        }
