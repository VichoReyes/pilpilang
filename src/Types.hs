{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Types where

import Syntax
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State (StateT, MonadState (put, get), MonadTrans (lift), execStateT)
import Control.Monad (forM_, forM, when)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (local, ask))
import qualified Data.Text as T
import Data.String (IsString)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ((<|))

tShow :: Show a => a -> Text
tShow = T.pack . show

class ColumnTypeProvider a where
    fillTypes :: a -> GEntity klass Text -> Either TypeError (GEntity klass (Text, Text))

newtype TypeError = TypeError
    { getTypeError :: Text
    } deriving (Eq, Ord, Show, Semigroup, Monoid, IsString)

type Type = Text

data ValidVal = ValidVal
    { vvContents :: Value
    , vvType :: NE.NonEmpty ValidType
    } deriving (Eq)

instance Show ValidVal where
    show (ValidVal c t) = "("<>show c<>": "<>show t<>")"

data ValidType
    = TInt
    | TBool
    | TString
    | TActor (GEntity ActorMarker (Text, ValidType))
    | TResource (GEntity ResourceMarker (Text, ValidType))

instance Eq ValidType where
    TInt == TInt = True
    TBool == TBool = True
    TString == TString = True
    TActor ent1 == TActor ent2 = entityName ent1 == entityName ent2
    TResource ent1 == TResource ent2 = entityName ent1 == entityName ent2
    _ == _ = False

instance Ord ValidType where
    compare TInt TInt = EQ
    compare TInt _ = LT
    compare TBool TInt = GT
    compare TBool TBool = EQ
    compare TBool _ = LT
    compare TString t2
      | t2 == TString = EQ
      | t2 `elem` [TInt, TBool] = GT
      | otherwise = LT
    compare (TActor _) (TResource _) = LT
    compare (TActor a1) (TActor a2) = compare (entityName a1) (entityName a2)
    compare (TActor _) _ = GT
    compare (TResource r1) (TResource r2) = compare (entityName r1) (entityName r2)
    compare (TResource _) _ = GT

instance Show ValidType where
    show TInt = "Int"
    show TBool = "Bool"
    show TString = "String"
    show (TActor ent) = T.unpack $ entityName ent
    show (TResource ent) = T.unpack $ entityName ent

isPrimitive :: ValidType -> Bool
isPrimitive (TActor _) = False
isPrimitive (TResource _) = False
isPrimitive _ = True

data EntityClass = EActor | EResource
    deriving (Eq, Ord, Show)

data TypedHeader
    = HPermission PermissionType (GEntity ActorMarker (Text, ValidType)) (GEntity ResourceMarker (Text, ValidType))
    | HDefinition Text [ValidType]
    deriving (Eq, Ord, Show)

data PartialInfo = PartialInfo
    { piEntities :: Map Text (EntityClass, Map Text Type) -- actor/resource name -> (Actor|Resource, column -> type)
    , piFunctions :: Map Text [Type] -- types of the arguments
    } deriving (Eq, Ord, Show)

instance Semigroup PartialInfo where
    ti1 <> ti2 = PartialInfo
        { piEntities = piEntities ti1 <> piEntities ti2
        , piFunctions = piFunctions ti1 <> piFunctions ti2 }

instance Monoid PartialInfo where
    mempty = PartialInfo
        { piEntities = M.empty
        , piFunctions = M.empty
        }

data TypeInfo = TypeInfo
    { entities :: Map Type ValidType -- actor/resource name -> Entity
    , functions :: Map Text [ValidType] -- types of the arguments
    , variables :: Map Text ValidType
    } deriving (Eq, Show)

instance Semigroup TypeInfo where
    ti1 <> ti2 = TypeInfo
        { entities = entities ti1 <> entities ti2
        , functions = functions ti1 <> functions ti2
        , variables = variables ti1 <> variables ti2 }

instance Monoid TypeInfo where
    mempty = TypeInfo
        { entities = M.empty
        , functions = M.empty
        , variables = M.empty }

type TypeGen a = StateT PartialInfo (Either TypeError) a

type TypeCheck a = ReaderT TypeInfo (Either TypeError) a

type TypedBody = ([Text], Predicate ValidVal)

runTypeChecker :: (ColumnTypeProvider a) => a -> AST -> Either TypeError (TypeInfo, Map TypedHeader TypedBody)
runTypeChecker provider ast = do
    partialGlobals <- execStateT (mkGlobals provider ast) mempty
    globals <- materialize partialGlobals ast
    preds <- runReaderT (cAssocs (astAssociations ast)) globals
    return (globals, preds)

materialize :: PartialInfo -> AST -> Either TypeError TypeInfo
materialize (PartialInfo ents funcs) ast = do
    let entNames = M.keys ents
    ents' <- mapM (genEnt ast) entNames
    let entPairs = zip entNames ents'
    funcs' <- traverse (mapM $ lookupType entPairs) funcs
    return mempty {entities = M.fromList entPairs, functions = funcs'}
        where
            lookupType entPairs typeName = case lookup typeName entPairs of
                Nothing -> Left . TypeError $ typeName<>" not found"
                Just b -> Right b

genEnt :: AST -> Text -> Either TypeError ValidType
genEnt _ "Int" = return TInt
genEnt _ "String" = return TString
genEnt _ "Bool" = return TBool
genEnt ast entName = do
    let entList = filter (\a -> entityName a == entName) $ astActors ast
    case entList of
        [] -> do
            case filter (\a -> entityName a == entName) $ astResources ast of
              [] -> Left . TypeError $ entName<>" not found"
              [ent] -> do
                    cols <- mapM (genEnt ast . snd) (entityColumns ent)
                    return $ TResource ent {entityColumns = zip (map fst $ entityColumns ent) cols}
              _ -> error "More than one definition? Shouldn't happen"
        [ent] -> do
            cols <- mapM (genEnt ast . snd) (entityColumns ent)
            return $ TActor ent {entityColumns = zip (map fst $ entityColumns ent) cols}
        _ -> error "More than one definition? Shouldn't happen"

-- TODO check for repeats
mkColumnMap :: GEntity klass (Text, Text) -> TypeGen (Map Text Type)
mkColumnMap entity = do
    let columns = entityColumns entity
    let columnMap = M.fromList columns
    if M.size columnMap == length columns
        then return columnMap
        else lift $ Left (TypeError ("duplicated column in "<>entityName entity))

mkTypeInfo :: [Actor] -> [Resource] -> TypeGen ()
mkTypeInfo acts ress = do
    forM_ acts (addEntity EActor)
    forM_ ress (addEntity EResource)

addEntity :: EntityClass -> GEntity a (Text, Type) -> TypeGen ()
addEntity klass a = do
    typeInfo <- get
    actorColumnsMap <- mkColumnMap a
    if M.member (entityName a) (piEntities typeInfo)
        then lift . Left . TypeError $ "entity "<>entityName a<>" defined twice"
        else put typeInfo {piEntities = M.insert (entityName a) (klass, actorColumnsMap) (piEntities typeInfo)}

mkGlobals :: ColumnTypeProvider a => a -> AST -> TypeGen ()
mkGlobals _provider ast = do
    mkTypeInfo (astActors ast) (astResources ast)
    forM_ (astAssociations ast) $ \assoc ->
        case assocHeader assoc of
            AHDef (Definition name args) -> addFunction name args
            _ -> return () -- skip permissions, they always have the same types

cTypeError :: Text -> TypeCheck a
cTypeError err = lift (Left (TypeError err))

cLookUp :: (TypeInfo -> Maybe a) -> Text -> TypeCheck a
cLookUp f err = do
    typeInfo <- ask
    case f typeInfo of
        Nothing -> cTypeError err
        Just a -> return a

cAssocs :: [Assoc] -> TypeCheck (Map TypedHeader TypedBody)
cAssocs associations = M.fromList <$> do
    forM associations $ \assoc -> do
        let predicate = assocDefinition assoc
        (localVars, header) <- mkLocalVars (assocHeader assoc)
        p <- local (\ti -> ti {variables = M.fromList localVars}) (cPredicate predicate)
        return (header, (map fst localVars, p))

mkLocalVars :: AssocHeader -> TypeCheck ([(Text, ValidType)], TypedHeader)
mkLocalVars (AHDef (Definition name vars)) = do
    let types = map typedVarType vars
    validTypes <- forM types getValType
    let header = HDefinition name validTypes
    let argNames = map typedVarName vars
    return (zip argNames validTypes, header)
mkLocalVars (AHPermission (Permission perm actorVar resourceVar)) = do
    let (actorName, actorType) = var2pair actorVar
    let (resourceName, resourceType) = var2pair resourceVar
    when (actorName == resourceName) $
        cTypeError ("actor "<>actorName<>" and resource "<>resourceName<>" can't have the same name")
    actorType' <- cLookUp (M.lookup actorType . entities) ("Actor "<>actorType<>" not defined")
    resourceType' <- cLookUp (M.lookup resourceType . entities) ("Resource "<>resourceType<>" not defined")
    case (actorType', resourceType') of
        (TActor actorEnt, TResource resourceEnt) ->
            let scopeVariables = [(actorName, actorType'), (resourceName, resourceType')]
                header = HPermission perm actorEnt resourceEnt
             in return (scopeVariables, header)
        (_ ,TResource _) -> cTypeError "when defining a permission, first argument should be an Actor"
        (TActor _, _) -> cTypeError $ tShow resourceType'<>": when defining a permission, second argument should be a Resource"
        (_, _) -> cTypeError "when defining a permission, first argument should be an Actor and second a Resource"

getValType :: Type -> TypeCheck ValidType
getValType "Int" = return TInt
getValType "String" = return TString
getValType "Bool" = return TBool
getValType typeName = do
    typeInfo <- ask
    case M.lookup typeName . entities $ typeInfo of
      Nothing -> lift . Left . TypeError $ typeName<>" doesn't exist"
      Just vt -> return vt

var2pair :: TypedVar -> (Text, Type)
var2pair var = (typedVarName var, typedVarType var)

cPredicate :: Predicate Value -> TypeCheck (Predicate ValidVal)
cPredicate (PredCall predName args) = do
    expectedTypes <- cLookUp (M.lookup predName . functions) $
        (predName<>" not defined")
    actualTypes <- forM args cValue
    when (length expectedTypes /= length actualTypes) $
        cTypeError (predName<>" call with wrong number of arguments")
    forM_ (zip expectedTypes actualTypes) $ \(ex, act) ->
        when (ex /= NE.head act)
            (cTypeError $ "in call to "<>predName<>": expected "<>tShow ex<>", found "<>tShow act)
    return . PredCall predName $ zipWith ValidVal args actualTypes
cPredicate PAlways = return PAlways
cPredicate (PAnd p1 p2) = PAnd <$> cPredicate p1 <*> cPredicate p2
cPredicate (POr p1 p2) = POr <$> cPredicate p1 <*> cPredicate p2
cPredicate (PEquals val1 val2) = do
    type1 <- cValue val1
    type2 <- cValue val2
    when (NE.head type1 /= NE.head type2) $ do
        -- TODO improve representation
        let val1' = tShow val1
        let val2' = tShow val2
        cTypeError ("mismatched types in "<>val1'<>" = "<>val2'<>
            ": first is "<>tShow (NE.head type1)<>" and second is "<>tShow (NE.head type2))
    return $ PEquals (ValidVal val1 type1) (ValidVal val2 type2)
cPredicate (PGreaterT val1 val2) = do
    type1 <- cValue val1
    when (NE.head type1 /= TInt) $
        cTypeError (tShow type1<>"doesn't support order comparison")
    type2 <- cValue val2
    when (NE.head type2 /= TInt) $
        cTypeError (tShow type2<>"doesn't support order comparison")
    return $ PGreaterT (ValidVal val1 type1) (ValidVal val2 type2)
-- reuse the last one
cPredicate (PLessT val1 val2) = cPredicate (PGreaterT val2 val1)

cValue :: Value -> TypeCheck (NE.NonEmpty ValidType)
cValue (VLitInt _) = pure . pure $ TInt
cValue (VLitBool _) = pure . pure $ TBool
cValue (VLitString _) = pure . pure $ TString
cValue (VVar varName) = pure <$> cLookUp (M.lookup varName . variables) (varName<>" not found")
cValue (VVarField varName varField) = do
    varType <- cValue varName
    lift $ maybe (Left (TypeError (tShow varType<>" has no field "<>varField))) (Right . (<|varType)) $
        propertyLookup varField (NE.head varType)

propertyLookup :: Text -> ValidType -> Maybe ValidType
propertyLookup k (TActor (Entity _ _ cols)) = lookup k cols
propertyLookup k (TResource (Entity _ _ cols)) = lookup k cols
propertyLookup _ _ = Nothing

addFunction :: Text -> [TypedVar] -> TypeGen ()
addFunction name args = do
    typeInfo <- get
    let currentFunctions = piFunctions typeInfo
    if M.member name currentFunctions
        then lift . Left . TypeError $ "function "<>name<>" defined twice"
        else put $ typeInfo {piFunctions = M.insert name (map typedVarType args) currentFunctions}

