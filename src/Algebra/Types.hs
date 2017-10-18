module Algebra.Types where

import Data.Text (Text)
import qualified Data.Map.Strict as Map


data AlgebraicExpr =
  IntegerExpr Integer |
  FloatExpr Double |
  SymbolExpr Text |
  TreeExpr Text [AlgebraicExpr]
  deriving Show

type SymbolTable = Map.Map Text AlgebraicExpr

data Kind = KInteger | KSymbol | KOperator Text

newtype ParseErr = Err Text deriving Show
