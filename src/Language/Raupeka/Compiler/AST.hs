{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Raupeka.Compiler.AST (
    RLit (..),
    RType (..),
    RTySig (..),
    RExpr (..),
    Binop (..),
    RExpr (..),
) where

import Data.Text (Text)
import Language.Raupeka.Types (Name)
import Text.Show.Deriving (deriveShow1)

type ModuleName = Text

type Ident = Text

data ImportEntity
  = ImportedVar Ident
  | ImportedType Ident
  deriving (Eq, Show)

data RauImportQualifiedType
  = RauImportQualified
  | RauImportUnqualified
  deriving (Eq, Show)

data RauImportDecl a
  = RauImportDecl {
    _rauImportDeclName :: ModuleName
  , _rauImportDeclAs   :: Maybe ModuleName -- ^ For qualified imports. If Nothing then it is qualified as the module name.
  , _rauImportDeclList :: Maybe ([a])
  , _rauImportDeclIsQualified :: RauImportQualifiedType -- ^ Whether the import is qualified or not
  } deriving (Eq, Show)

-- data RauMod
--   = RauMod {
--     _rauModName    :: Maybe (Text) -- ^ Module name - if Nothing then Main
-- --  , _rauModExports :: Maybe ([Decl]) -- ^ List of exported functions
-- --  , _rauModImports :: [Decl] -- ^ List of imported modules
--   , _rauModSource  :: Text -- ^ Source code
-- --  , _rauModDecls   :: [Decl] -- ^ List of declarations
--   } deriving (Eq, Show)

data RLit
    = LInt Integer
    | LBool Bool
    | LStr Text
    | LList [RExpr]
    deriving (Eq, Show)

data RType = RType
    { tyIdent :: Name
    , tyParametric :: Bool
    , tyParams :: Maybe [RType]
    }
    deriving (Eq, Show)

data RTySig
    = Inline [RType]
    | TopLev Name [RType]
    deriving (Eq, Show)

newtype Mu f = Mu (f (Mu f))

-- data RExprF a
--     = Var Name
--     | App a a
--     | Let Name a a
--     | Lit RLit
--     | Sig RTySig
--     | Lam Name a
--     | Fix a
--     | If RExpr a a
--     | Op Binop a a
--     deriving (Functor)

data RExpr
    = Var Name
    | App RExpr RExpr
    | Let Name RExpr RExpr
    | Lit RLit
    | Sig RTySig
    | Lam Name RExpr
    | Fix RExpr
    | If RExpr RExpr RExpr
    | Op Binop RExpr RExpr
    deriving (Show, Eq)

-- $(deriveShow1 ''RExprF)

-- type RExpr = Mu RExprF

-- type RExpr' = Cofree RExprF

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
    | Map
    deriving (Eq, Ord, Show)
