{-# LANGUAGE OverloadedStrings #-}

module Conversion where

import Syntax
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Control.Monad.Reader (Reader)
import Data.Foldable (fold)
import Control.Monad.RWS (asks)

type Conversor = Reader (AST, [(Text, (Char, Int))])

asdf :: Conversor Text
asdf = do
    ast <- asks fst
    let permissions = mapMaybe fromPermission $ astAssociations ast
    fold <$> traverse qwer permissions

qwer :: (Permission, Predicate) -> Conversor Text
qwer (Permission perm act res, pred) = do
    let resType = asks fst
    return $ "CREATE POLICY todo_name ON "<>entityTable (typedVarType res)


fromPermission :: Assoc -> Maybe (Permission, Predicate)
fromPermission assoc = case assocHeader assoc of
    AHPermission per -> Just (per, assocDefinition assoc)
    AHDef _ -> Nothing

