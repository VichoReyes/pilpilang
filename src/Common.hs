{-# LANGUAGE TemplateHaskell #-}

module Common where

import Data.Text (Text, unpack)
import Lens.Micro.Platform ( makeLenses )
import Data.Map (Map)

data EntityClass = EActor | EResource
    deriving (Eq, Show, Ord)

data GEntity withKeys without = GEntity
    { _entityName :: Text
    , _entityTable :: Text
    , _entityKeys :: [Text]
    , _entityColumns :: Map Text (ColumnType withKeys without)
    , _entityClass :: EntityClass
    } deriving (Eq, Show, Ord)

type ColumnType withKeys without = Either (withKeys, [Text]) without

data GAssoc headerType valueType = GAssoc
    { _assocHeader :: GAssocHeader headerType
    , _assocDefinition :: GPredicate valueType
    } deriving (Eq, Show, Ord)

type GAssocHeader headerType = Either (GPermission headerType) (GDefinition headerType)

data GPermission headerType = GPermission
    { _permissionType :: PermissionType
    , _permissionActor :: (Text, headerType)
    , _permissionResource :: (Text, headerType)
    , _permissionExtraArgs :: [(Text, headerType)]
    } deriving (Eq, Show, Ord)

data PermissionType = PCanSelect | PCanInsert | PCanUpdate | PCanDelete | PCanAnything
    deriving (Eq, Show, Ord)

data GDefinition headerType = GDefinition
    { _defName :: Text
    , _defArgs :: [(Text, headerType)]
    , _defExtraArgs :: [(Text, headerType)]
    } deriving (Eq, Show, Ord)

data GPredicate valueType
    = PredCall Text [GValue valueType]
    | PAlways
    | PAnd (GPredicate valueType) (GPredicate valueType)
    | POr (GPredicate valueType) (GPredicate valueType)
    | PEquals (GValue valueType) (GValue valueType)
    | PGreaterT (GValue valueType) (GValue valueType)
    | PLessT (GValue valueType) (GValue valueType)
    deriving (Eq, Show, Ord)

data GValue valueType
    = VVar
        { _valName :: Text
        , _valType :: valueType }
    | VVarField
        { _valObject :: GValue valueType
        , _valField :: Text
        , _valType :: valueType }
    | VLiteral Literal
    -- literals
    deriving (Eq, Ord)

instance Show (GValue valueType) where
    show (VVar t _) = unpack t
    show (VVarField v f _) = show v ++ "." ++ unpack f
    show (VLiteral l) = show l

data Literal
    = LitInt Int
    | LitBool Bool
    | LitString Text
    deriving (Eq, Ord, Show)

makeLenses ''GAssoc
makeLenses ''GEntity
makeLenses ''GValue
makeLenses ''GDefinition
makeLenses ''GPermission
