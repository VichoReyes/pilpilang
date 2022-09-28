{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Types where

import Syntax
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State (StateT, execStateT, evalStateT)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Text as T
import Data.String (IsString)
import Data.Maybe (fromJust)
import Control.Monad.RWS
    ( forM_,
      forM,
      when,
      gets,
      MonadState(put, get),
      MonadTrans(lift),
      asks,
      MonadReader(local, ask) )

tShow :: Show a => a -> Text
tShow = T.pack . show

class ColumnTypeProvider a where
    fillTypes :: a -> GEntity klass Text -> Either TypeError (GEntity klass (Text, Text))

data ValidVar = ValidVar
    { vvName :: Text
    , vvType :: ValidType
    } deriving (Eq, Show)

data TypedHeader = THPermission TypedPerm | THDef TypedDef

data TypedPerm = TypedPerm
    { permissionType :: PermissionType
    , permissionActor :: ValidVar
    , permissionResource :: ValidVar
    } deriving (Eq, Show)

data TypedDef = TypedDef
    { defName :: Text
    , defArgs :: [ValidVar]
    } deriving (Eq, Show)

newtype TypeError = TypeError
    { getTypeError :: Text
    } deriving (Eq, Ord, Show, Semigroup, Monoid, IsString)

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

instance Show ValidType where
    show TInt = "Int"
    show TBool = "Bool"
    show TString = "String"
    show (TActor ent) = T.unpack $ entityName ent
    show (TResource ent) = T.unpack $ entityName ent

type Type = Text

data EntityClass = EActor | EResource
    deriving (Eq, Ord, Show)

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
        , variables = M.empty
        }

type TypeGen a = StateT TypeInfo (Either TypeError) a

type TypeCheck a = ReaderT TypeInfo (Either TypeError) a

runTypeChecker :: (ColumnTypeProvider a) => a -> AST -> Either TypeError ()
runTypeChecker provider ast = do
    globals <- execStateT (mkGlobals provider ast) mempty
    runReaderT (cAssocs (astAssociations ast)) globals

-- TODO check for repeats
mkColumnMap :: GEntity klass (Text, Text) -> TypeGen (Map Text ValidType)
mkColumnMap entity = do
    let columns = entityColumns entity
    let columnMap = M.fromList columns
    if M.size columnMap == length columns
        then forM columnMap getValType
        else lift $ Left (TypeError ("duplicated column in "<>entityName entity))

getValType :: Type -> TypeGen ValidType
getValType "Int" = return TInt
getValType "String" = return TString
getValType "Bool" = return TBool
getValType typeName = do
    typeInfo <- get
    case M.lookup typeName . entities $ typeInfo of
      Nothing -> lift . Left . TypeError $ typeName<>" doesn't exist"
      Just vt -> return vt

getValType' :: Type -> TypeCheck ValidType
getValType' typeName = do
    typeInfo <- ask
    lift $ evalStateT (getValType typeName) typeInfo

mkTypeInfo :: [Actor] -> [Resource] -> TypeGen ()
mkTypeInfo acts ress = do
    forM_ acts (addEntity EActor)
    forM_ ress (addEntity EResource)

addEntity :: EntityClass -> GEntity a (Text, Type) -> TypeGen ()
addEntity klass ent = do
    typeInfo <- get
    actorColumnsMap <- mkColumnMap ent
    if M.member (entityName ent) (entities typeInfo)
        then lift . Left . TypeError $ "entity "<>entityName ent<>" defined twice"
        else do
            let ent' = ent {entityColumns = M.toList actorColumnsMap}
            let result = case klass of
                  EActor -> TActor ent'
                  EResource -> TResource ent'
            put typeInfo {entities = M.insert (entityName ent) result (entities typeInfo)}

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

cAssocs :: [Assoc] -> TypeCheck ()
cAssocs associations = do
    forM_ associations $ \assoc -> do
        let predicate = assocDefinition assoc
        localVars <- mkLocalVars (assocHeader assoc)
        local (localVars<>) (cPredicate predicate)

mkLocalVars :: AssocHeader -> TypeCheck TypeInfo
mkLocalVars (AHDef (Definition _ vars)) = do
    let varsMap = M.fromList $ map var2pair vars
    valVarsMap <- forM varsMap getValType'
    return mempty {variables = valVarsMap}
mkLocalVars (AHPermission (Permission _ actorVar resourceVar)) = do
    let (actorName, actorType) = var2pair actorVar
    let (resourceName, resourceType) = var2pair resourceVar
    when (actorName == resourceName) $
        cTypeError ("actor "<>actorName<>" and resource "<>resourceName<>" can't have the same name")
    actorType' <- cLookUp (M.lookup actorType . entities) ("Actor "<>actorType<>" not defined")
    when (actorType' /= TActor (actor actorType undefined undefined)) $
        cTypeError "when defining a permission, first argument should be an Actor"
    resourceType' <- cLookUp (M.lookup resourceType . entities) ("Resource "<>resourceType<>" not defined")
    when (resourceType' /= TResource (resource resourceType undefined undefined)) $
        cTypeError "when defining a permission, second argument should be an Resource"
    return mempty {variables = M.fromList [(actorName, actorType'), (resourceName, resourceType')]}

var2pair :: TypedVar -> (Text, Type)
var2pair var = (typedVarName var, typedVarType var)

cPredicate :: Predicate -> TypeCheck ()
cPredicate (PredCall predName args) = do
    expectedTypes <- cLookUp (M.lookup predName . functions) $
        (predName<>" not defined")
    actualTypes <- forM args cValue
    when (length expectedTypes /= length actualTypes) $
        cTypeError (predName<>" call with wrong number of arguments")
    forM_ (zip expectedTypes actualTypes) $ \(ex, act) ->
        when (ex /= act)
            (cTypeError $ "in call to "<>predName<>": expected "<>tShow ex<>", found "<>tShow act)
cPredicate PAlways = return ()
cPredicate (PAnd p1 p2) = cPredicate p1 >> cPredicate p2
cPredicate (POr p1 p2) = cPredicate p1 >> cPredicate p2
cPredicate (PEquals val1 val2) = do
    type1 <- cValue val1
    type2 <- cValue val2
    when (type1 /= type2) $ do
        -- TODO improve representation
        let val1' = T.pack (show val1)
        let val2' = T.pack (show val2)
        cTypeError ("mismatched types in "<>val1'<>" = "<>val2'<>
            ": first is "<>tShow type1<>" and second is "<>tShow type2)
    when (type1 `notElem` [TString, TInt, TBool]) $
        cTypeError ("cannot compare "<>tShow type1<>"s, can only compare Strings, Ints and Bools")
cPredicate (PGreaterT val1 val2) = do
    type1 <- cValue val1
    when (type1 /= TInt) $
        cTypeError (tShow type1<>"doesn't support order comparison")
    type2 <- cValue val2
    when (type2 /= TInt) $
        cTypeError (tShow type2<>"doesn't support order comparison")
-- reuse the last one
cPredicate (PLessT val1 val2) = cPredicate (PGreaterT val1 val2)

cValue :: Value -> TypeCheck ValidType
cValue (VLitInt _) = return TInt
cValue (VLitBool _) = return TBool
cValue (VLitString _) = return TString
cValue (VVar varName) = cLookUp (M.lookup varName . variables) (varName<>" not found")
cValue (VVarField varName varField) = do
    varType <- cValue varName
    lift $ maybe (Left (TypeError (tShow varType<>" has no field "<>varField))) Right $
        entLookup varField varType

entLookup :: Text -> ValidType -> Maybe ValidType
entLookup k (TActor (Entity _ _ cols)) = lookup k cols
entLookup k (TResource (Entity _ _ cols)) = lookup k cols
entLookup _ _ = Nothing

addFunction :: Text -> [TypedVar] -> TypeGen ()
addFunction name args = do
    typeInfo <- get
    let currentFunctions = functions typeInfo
    if M.member name currentFunctions
        then lift . Left . TypeError $ "function "<>name<>" defined twice"
        else do
            validArgs <- forM args $ \arg -> do
                getValType $ typedVarType arg
            put $ typeInfo {functions = M.insert name validArgs currentFunctions}

