module Algebra where

import Data.Text(Text)
import qualified Data.Map.Strict as Map

import Algebra.Types

eval :: SymbolTable -> AlgebraicExpr -> AlgebraicExpr
eval st s@(SymbolExpr name) = Map.findWithDefault s name st
eval _ other                = other

automaticSimplify :: AlgebraicExpr -> AlgebraicExpr
automaticSimplify (TreeExpr c exprs) = w
  where v = TreeExpr c $ fmap automaticSimplify exprs
        w = applyRules v
        applyRules = id

automaticSimplify atomicExpr = atomicExpr


kind :: AlgebraicExpr -> Kind
kind (TreeExpr op _) = KOperator op
kind (IntegerExpr i) = KInteger
kind (SymbolExpr s) = KSymbol

numberOfOperands :: AlgebraicExpr -> Maybe Int
numberOfOperands (TreeExpr op exprs) = Just $ length exprs
numberOfOperands _                   = Nothing

operand :: AlgebraicExpr -> Int -> Maybe AlgebraicExpr
operand (TreeExpr op exprs) n
  | n >= length exprs = Nothing
  | otherwise         = Just $ exprs !! n

operand _ _ = Nothing

construct :: Text -> [AlgebraicExpr] -> AlgebraicExpr
construct = TreeExpr
