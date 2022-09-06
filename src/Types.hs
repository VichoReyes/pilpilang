{-# LANGUAGE OverloadedStrings #-}
module Types where

import Syntax
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State (StateT, MonadState (put, get), MonadTrans (lift), execStateT)
import Control.Monad (forM_, forM, when)
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (local, ask))

class ColumnTypeProvider a where
    fillTypes :: a -> GEntity klass Text -> Either TypeError (GEntity klass (Text, Text))

data TypedAssoc -- TODO

newtype TypeError = TypeError
    { getTypeError :: Text
    } deriving (Eq, Ord, Show)

type Type = Text

data EntityClass = EActor | EResource
    deriving (Eq, Ord, Show)

data TypeInfo = TypeInfo
    { entities :: Map Text (EntityClass, Map Text Type) -- actor/resource name -> (Actor|Resource, column -> type)
    , functions :: Map Text [Type] -- types of the arguments
    , variables :: Map Text Type
    } deriving (Eq, Ord, Show)

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
mkColumnMap :: GEntity klass (Text, Text) -> TypeGen (Map Text Type)
mkColumnMap entity = do
    let columns = entityColumns entity
    let columnMap = M.fromList columns
    if M.size columnMap == length columns
        then return columnMap
        else lift $ Left (TypeError ("duplicated column in "<>entityName entity))

mkTypeInfo :: [TypedActor] -> [TypedResource] -> TypeGen ()
mkTypeInfo acts ress = do
    forM_ acts (addEntity EActor)
    forM_ ress (addEntity EResource)

addEntity :: EntityClass -> GEntity a (Text, Type) -> TypeGen ()
addEntity klass a = do
    typeInfo <- get
    actorColumnsMap <- mkColumnMap a
    if M.member (entityName a) (entities typeInfo)
        then lift . Left . TypeError $ "entity "<>entityName a<>" defined twice"
        else put typeInfo {entities = M.insert (entityName a) (klass, actorColumnsMap) (entities typeInfo)}

mkGlobals :: ColumnTypeProvider a => a -> AST -> TypeGen ()
mkGlobals provider ast = do
    typedActors <- forM (astActors ast) (lift . fillTypes provider)
    typedResources <- forM (astResources ast) (lift . fillTypes provider)
    mkTypeInfo typedActors typedResources
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
mkLocalVars (AHDef (Definition _ vars)) =
    let varsMap = M.fromList $ map var2pair vars
    in return mempty {variables = varsMap}
mkLocalVars (AHPermission (Permission _ actorVar resourceVar)) = do
    let (actorName, actorType) = var2pair actorVar
    let (resourceName, resourceType) = var2pair resourceVar
    when (actorName == resourceName) $
        cTypeError ("actor "<>actorName<>" and resource "<>resourceName<>" can't have the same name")
    (actorClass, _) <- cLookUp (M.lookup actorName . entities) ("Actor "<>actorName<>" not defined")
    when (actorClass /= EActor) $
        cTypeError "when defining a permission, first argument should be an Actor"
    (resourceClass, _) <- cLookUp (M.lookup resourceName . entities) ("Resource "<>resourceName<>" not defined")
    when (resourceClass /= EResource) $
        cTypeError "when defining a permission, second argument should be an Resource"
    return mempty {variables = M.fromList [(actorName, actorType), (resourceName, resourceType)]}

var2pair :: TypedVar -> (Text, Type)
var2pair var = (typedVarName var, typedVarType var)

cPredicate :: Predicate -> TypeCheck ()
cPredicate (PredCall predName args) = do
    expectedTypes <- cLookUp (M.lookup predName . functions) $ 
        (predName<>" not defined")
    actualTypes <- forM args cValue
    when (length expectedTypes /= length actualTypes) $
        cTypeError (predName<>" call with wrong number of arguments")
    forM_ (zip expectedTypes actualTypes) $ \(exp, act) ->
        when (exp /= act)
            (cTypeError $ "in call to "<>predName<>": expected "<>exp<>", found "<>act)

cValue :: Value -> TypeCheck Type
cValue (VLitInt _) = return "Int"
cValue (VLitBool _) = return "Bool"
cValue (VLitString _) = return "String"
cValue (VVar varName) = cLookUp (M.lookup varName . variables) (varName<>" not found")
cValue (VVarField varName varField) = cLookUp lookUpField (varName<>"."<>varField<>" not found")
    where
        lookUpField typeInfo = do
            varType <- M.lookup varName (variables typeInfo)
            fields <- M.lookup varType (entities typeInfo)
            M.lookup varField (snd fields)

addFunction :: Text -> [TypedVar] -> TypeGen ()
addFunction name args = do
    typeInfo <- get
    let currentFunctions = functions typeInfo
    if M.member name currentFunctions
        then lift . Left . TypeError $ "function "<>name<>" defined twice"
        else put $ typeInfo {functions = M.insert name (map typedVarType args) currentFunctions}

