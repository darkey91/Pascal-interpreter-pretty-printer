
module Interpreter where

import Data.Map.Strict
import Code

import Parser.PascalGrammar

data Err
  = LogicErr
  | InterpretErr
  | TypeCastErr

type Scope = Map String Expr

type Store = [Scope]




