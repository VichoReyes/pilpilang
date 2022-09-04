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

data TypeInfo = TypeInfo
    { actors :: Map Text (Map Text Type) -- actor name -> column -> type
    , resources :: Map Text (Map Text Type) -- same
    , functions :: Map Text [Type] -- types of the arguments
    } deriving (Eq, Ord, Show)

emptyTypeInfo :: TypeInfo
emptyTypeInfo = TypeInfo
    { actors = M.empty
    , resources = M.empty
    , functions = M.empty
    }

type TypingMonad a = StateT TypeInfo (Either TypeError) a

-- TODO check for repeats
mkColumnMap :: GEntity klass (Text, Text) -> Either TypeError (Map Text Type)
mkColumnMap entity = do
    let columns = entityColumns entity
    let columnMap = M.fromList columns
    if M.size columnMap == length columns
        then return columnMap
        else Left (TypeError ("duplicated column in "<>entityName entity))

mkTypeInfo :: [TypedActor] -> [TypedResource] -> TypingMonad ()
mkTypeInfo acts ress = do
    forM_ acts addActor
    forM_ ress addResource

addActor :: TypedEntity a -> TypingMonad ()
addActor a = do
    typeInfo <- get
    actorColumnsMap <- lift (mkColumnMap a)
    if M.member (entityName a) (actors typeInfo) || M.member (entityName a) (resources typeInfo)
        then undefined
        else put typeInfo {actors = M.insert (entityName a) actorColumnsMap (actors typeInfo)}

addResource :: TypedResource -> TypingMonad ()
addResource r = do
    typeInfo <- get
    actorColumnsMap <- lift (mkColumnMap r)
    if M.member (entityName r) (actors typeInfo) || M.member (entityName r) (resources typeInfo)
        then undefined
        else put typeInfo {resources = M.insert (entityName r) actorColumnsMap (resources typeInfo)}

typeCheckAST :: ColumnTypeProvider a => a -> AST -> TypingMonad ()
typeCheckAST provider ast = do
    typedActors <- forM (astActors ast) (lift . fillTypes provider)
    typedResources <- forM (astResources ast) (lift . fillTypes provider)
    mkTypeInfo typedActors typedResources
