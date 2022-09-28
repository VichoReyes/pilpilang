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

tShow :: Show a => a -> Text
tShow = T.pack . show

class ColumnTypeProvider a where
    fillTypes :: a -> GEntity klass Text -> Either TypeError (GEntity klass (Text, Text))

data TypedAssoc -- TODO

newtype TypeError = TypeError
    { getTypeError :: Text
    } deriving (Eq, Ord, Show, Semigroup, Monoid, IsString)

type Type = Text

data ValidVar = ValidVar
    { vvName :: Text
    , vvType :: ValidType
    } deriving (Eq, Show)

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

data EntityClass = EActor | EResource
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

runTypeChecker :: (ColumnTypeProvider a) => a -> AST -> Either TypeError ()
runTypeChecker provider ast = do
    partialGlobals <- execStateT (mkGlobals provider ast) mempty
    globals <- materialize partialGlobals ast
    runReaderT (cAssocs (astAssociations ast)) globals

materialize :: PartialInfo -> AST -> Either TypeError TypeInfo
materialize (PartialInfo ents funcs) ast = do
    ents' <- mapM genEnt ents
    funcs' <- undefined
    return mempty {entities = ents', functions = funcs'}

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

cAssocs :: [Assoc] -> TypeCheck ()
cAssocs associations = do
    forM_ associations $ \assoc -> do
        let predicate = assocDefinition assoc
        localVars <- mkLocalVars (assocHeader assoc)
        local (localVars<>) (cPredicate predicate)

mkLocalVars :: AssocHeader -> TypeCheck TypeInfo
mkLocalVars (AHDef (Definition _ vars)) = do
    let varsMap = M.fromList $ map var2pair vars
    valVarsMap <- forM varsMap getValType
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
        propertyLookup varField varType

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

