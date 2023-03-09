module Language.Raupeka.AST ( RLit   (..)
                            , RType  (..)
                            , RTySig (..)
                            , RExpr  (..)
                            , Binop (..)) where

import Language.Raupeka.Types (Name)
import Data.Text (Text)

data RLit
  = LInt  Integer
  | LBool Bool
  | LStr  Text
  | LList [RExpr]
  deriving (Eq, Ord, Show)

data RType = RType { tyIdent      :: Name
                   , tyParametric :: Bool
                   , tyParams     :: Maybe [RType] }
             deriving (Eq, Ord, Show)

data RTySig
  = Inline [RType]
  | TopLev Name [RType]
  deriving (Eq, Ord, Show)

data RExpr
  = Var Name
  | App RExpr RExpr
  | Let Name  RExpr RExpr
  | Lit RLit
  | Sig RTySig
  | Lam Name  RExpr
  | Fix RExpr
  | If  RExpr RExpr RExpr
  | Op  Binop RExpr RExpr
  deriving (Eq, Ord, Show)

data Binop
  = Add
  | Sub
  | Mul
  | Div
  | Eql
  | Gtr
  | Lss
  | Gte
  | Lse
  | Cmp
  deriving (Eq, Ord, Show)
  
  
  
