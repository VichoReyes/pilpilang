module Types where

import Syntax (GEntity)
import Data.Text (Text)

class ColumnTypeProvider a where
    fillTypes :: a -> GEntity klass Text -> IO (GEntity klass (Text, Text))

