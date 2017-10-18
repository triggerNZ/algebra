{-# LANGUAGE OverloadedStrings #-}

module Main where

import Algebra
import Algebra.Types
import Algebra.Sexp

import qualified Data.Map.Strict as Map

import Prelude hiding (getLine, putStr, print)

import Data.Text.IO (getLine)
import System.IO (stdout, hFlush, putStr, print)

import Data.Text(Text)

main :: IO ()
main = repl

flush :: IO ()
flush = hFlush stdout

repl :: IO ()
repl = do
  let symbolTable = Map.fromList [("a", IntegerExpr 3)]
  putStr "> "
  flush
  line <- getLine
  case parse line of
    Left err   -> print err
    Right expr -> print $ eval symbolTable expr
  repl
