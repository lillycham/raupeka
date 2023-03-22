{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Raupeka.Type.Pretty () where

import Language.Raupeka.Pretty
import Language.Raupeka.Type.Checker

instance RaupekaPretty Scheme where
  rpretty = schemePretty

instance RaupekaPretty Type where
  rpretty = typePretty

instance RaupekaPretty TypeError where
  rpretty = errPretty

schemePretty :: Scheme -> String
schemePretty (Forall [] t) = typePretty t
schemePretty (Forall xt t) = "∀ " <> unwords (map unvar xt) <> ". " <> typePretty t

typePretty :: Type -> String
typePretty (TypeVar (TV v)) = v
typePretty (TypeCon c) = c
typePretty (TypeArr a b) = typePretty a <> " -> " <> typePretty b
typePretty (TypePCon p) = ptypeCon p <> " " <> unwords (map typePretty $ ptypeArgs p)

errPretty :: TypeError -> String
errPretty (UnificationFail a b) = "Unification failed: expected a value of type " <> typePretty b <> ", but found " <> typePretty a
errPretty (InfiniteType a b) = "Infinite type: " <> unvar a <> " and " <> typePretty b
errPretty (UnboundVariable v) = "Variable not in scope: " <> v
