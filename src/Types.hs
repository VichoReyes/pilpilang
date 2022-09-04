{-# LANGUAGE OverloadedStrings #-}
module Types where

import Syntax (GEntity (entityColumns, entityName), TypedActor, TypedResource, AST (astActors, astResources))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State (StateT, MonadState (put, get), MonadTrans (lift))
import Control.Monad (forM_, forM)

class ColumnTypeProvider a where
    fillTypes :: a -> GEntity klass Text -> Either TypeError (GEntity klass (Text, Text))

data TypedAssoc -- TODO

{-
data TypedAST = TypedAST
    { tastActors :: [TypedActor]
    , tastResources :: [TypedResource]
    , tastAssocs :: [TypedAssoc]
    } -- deriving (Eq, Ord, Show)
-}

newtype TypeError = TypeError
    { getTypeError :: Text
    } deriving (Eq, Ord, Show)

type Type = Text

type TypedEntity a = GEntity a (Text, Text)

data EntityClass = EActor | EResource
    deriving (Eq, Ord, Show)

data TypeInfo = TypeInfo
    { entities :: Map Text (EntityClass, Map Text Type) -- actor/resource name -> (Actor|Resource, column -> type)
    , functions :: Map Text [Type] -- types of the arguments
    } deriving (Eq, Ord, Show)

emptyTypeInfo :: TypeInfo
emptyTypeInfo = TypeInfo
    { entities = M.empty
    , functions = M.empty
    }

type TypingMonad a = StateT TypeInfo (Either TypeError) a

-- TODO check for repeats
mkColumnMap :: GEntity klass (Text, Text) -> TypingMonad (Map Text Type)
mkColumnMap entity = do
    let columns = entityColumns entity
    let columnMap = M.fromList columns
    if M.size columnMap == length columns
        then return columnMap
        else lift $ Left (TypeError ("duplicated column in "<>entityName entity))

mkTypeInfo :: [TypedActor] -> [TypedResource] -> TypingMonad ()
mkTypeInfo acts ress = do
    forM_ acts (addEntity EActor)
    forM_ ress (addEntity EResource)

addEntity :: EntityClass -> TypedEntity a -> TypingMonad ()
addEntity klass a = do
    typeInfo <- get
    actorColumnsMap <- mkColumnMap a
    if M.member (entityName a) (entities typeInfo)
        then lift . Left . TypeError $ "entity "<>entityName a<>" defined twice"
        else put typeInfo {entities = M.insert (entityName a) (klass, actorColumnsMap) (entities typeInfo)}

typeCheckAST :: ColumnTypeProvider a => a -> AST -> TypingMonad ()
typeCheckAST provider ast = do
    typedActors <- forM (astActors ast) (lift . fillTypes provider)
    typedResources <- forM (astResources ast) (lift . fillTypes provider)
    mkTypeInfo typedActors typedResources
