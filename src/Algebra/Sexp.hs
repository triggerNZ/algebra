module Algebra.Sexp where

import Algebra.Types
import Data.SCargot.Repr.WellFormed
import Data.SCargot.Language.HaskLike
import Data.SCargot.Parse
import Data.Either
import Data.Text
import Data.Bifunctor

data SexpError = StringsNotSupported | NotWellFormed | ParseError deriving (Eq, Show)

type ConvertedSexp a = Either SexpError a


fromSexpr :: WellFormedSExpr HaskLikeAtom -> ConvertedSexp AlgebraicExpr

fromSexpr (WFSAtom (HSIdent txt))  = Right $ SymbolExpr txt
fromSexpr (WFSAtom (HSString txt)) = Left StringsNotSupported
fromSexpr (WFSAtom (HSInt n))      = Right $ IntegerExpr n
fromSexpr (WFSAtom (HSFloat f))    = Right $ FloatExpr f

fromSexpr (WFSList (WFSAtom (HSIdent txt) : xs)) = do
  subexprList <- traverse fromSexpr xs
  return $ TreeExpr txt subexprList

fromSexpr _ = Left NotWellFormed


parse :: Text -> ConvertedSexp AlgebraicExpr
parse text = let
  parsed = decodeOne (asWellFormed haskLikeParser) text
  in
    first (const ParseError) parsed >>= fromSexpr
