{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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
    { _entities :: Map Type ValidType -- actor/resource name -> Entity
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
    funcs' <- traverse (mapM $ lookupType entPairs) funcs
    return mempty <&> entities .~ M.fromList entPairs
                  <&> functions .~ funcs'
        where
            lookupType entPairs typeName = case lookup typeName entPairs of
                Nothing -> Left . TypeError $ typeName<>" not found"
                Just b -> Right b

genEnt :: AST -> Text -> Either TypeError ValidType
genEnt _ "Int" = return (TPrimitive TInt)
genEnt _ "String" = return (TPrimitive TString)
genEnt _ "Bool" = return (TPrimitive TBool)
genEnt ast entName = TEntity . NonPrimitive <$> do
    case ast ^? astEntities . each . filtered (\a -> a ^. entityName == entName) of
        Nothing -> typeFail $ entName <>" not found"
        Just ent -> do
            return $ columnsMap colWithKeys colWithout ent
    where
        colWithout :: Text -> Primitive
        colWithout colType = case fromRight undefined $ genEnt ast colType of
            TPrimitive t -> t
            TEntity _ -> error "there should be keys"
        colWithKeys :: [Text] -> Text -> NonPrimitive
        colWithKeys keysList colType = case fromRight undefined $ genEnt ast colType of
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

cAssocs :: [Assoc] -> TypeCheck TypedAST
cAssocs = undefined
{-
cAssocs associations = M.fromList <$> do
    forM associations $ \assoc -> do
        let predicate = assocDefinition assoc
        (localVars, header) <- mkLocalVars (assocHeader assoc)
        p <- local (\ti -> ti {variables = M.fromList localVars}) (cPredicate predicate)
        return (header, (map fst localVars, p))

mkLocalVars :: AssocHeader -> TypeCheck ([(Text, ValidType)], TypedHeader)
mkLocalVars (Left (GDefinition name vars)) = do
    let types = map typedVarType vars
    validTypes <- forM types getValType
    let header = HDefinition name validTypes
    let argNames = map typedVarName vars
    return (zip argNames validTypes, header)
mkLocalVars (Right (GPermission perm actorVar resourceVar)) = do
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
-}

{-
TODO: Unify with genEnt, which does the same
getValType :: Type -> TypeCheck ValidType
getValType "Int" = return TInt
getValType "String" = return TString
getValType "Bool" = return TBool
getValType typeName = do
    typeInfo <- ask
    case M.lookup typeName . entities $ typeInfo of
      Nothing -> lift . Left . TypeError $ typeName<>" doesn't exist"
      Just vt -> return vt
-}

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
