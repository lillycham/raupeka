{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Raupeka.Type.Checker where

import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import Data.Map qualified as Map
import Data.Monoid ()
import Data.Set qualified as Set
import Language.Raupeka.Compiler.AST

type Var = String

newtype TypeVar = TV String
  deriving (Show, Eq, Ord)

-- | A parameterized type, e.g. Maybe a, polymorphic over one or more type variables.
data PCon = PCon
  { ptypeCon :: String,
    ptypeArgs :: [Type]
  }
  deriving (Show, Eq, Ord)

arrToList :: Type -> [Type]
arrToList (TypeArr a b) = a : arrToList b
arrToList x = [x]

unvar :: TypeVar -> String
unvar (TV x) = x

data Type
  = TypeVar TypeVar
  | TypeCon String
  | TypeArr Type Type
  | TypePCon PCon
  deriving (Show, Eq, Ord)

infixr 9 `TypeArr`

data Scheme = Forall [TypeVar] Type
  deriving (Show, Eq, Ord)

schemetype :: Scheme -> Type
schemetype (Forall _ xs) = xs

hasUnused :: Scheme -> Bool
hasUnused (Forall vs t) = not $ null $ Set.fromList vs `Set.difference` freshTyVar t

warnUnused :: Scheme -> Scheme
warnUnused s@(Forall vs _) =
  if hasUnused s
    then error $ "Unused type variables: " <> show vs
    else s

typeInt :: Type
typeInt = TypeCon "Int"

typeBool :: Type
typeBool = TypeCon "Bool"

typeList :: Type
typeList = TypeCon "List" `TypeArr` TypeVar (TV "a")

-- | Type Inference
newtype TypeEnv = TypeEnv (Map.Map Var Scheme)
  deriving (Semigroup, Monoid, Show)

extractEnv :: TypeEnv -> Map.Map Var Scheme
extractEnv (TypeEnv m) = m

data Unique = Unique {count :: Int}

type Infer = ExceptT TypeError (State Unique)

type Subst = Map.Map TypeVar Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TypeVar Type
  | UnboundVariable String
  deriving (Show)

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err -> Left err
  Right res -> Right $ closeOver res

closeOver :: (Map.Map TypeVar Type, Type) -> Scheme
closeOver (sub, ty) = normalize sc
  where
    sc = generalize emptyTyenv (apply sub ty)

initUnique :: Unique
initUnique = Unique {count = 0}

extend :: TypeEnv -> (Var, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env

primTypes :: [(Var, Scheme)]
primTypes =
  [ ("id", Forall [TV "a"] (TypeVar (TV "a") `TypeArr` TypeVar (TV "a"))),
    ("const", Forall [TV "a", TV "b"] (TypeVar (TV "a") `TypeArr` TypeVar (TV "b") `TypeArr` TypeVar (TV "a"))),
    -- , ("flip", Forall [TV "a", TV "b", TV "c"] (TypeVar (TV "a") `TypeArr` TypeVar (TV "b") `TypeArr` TypeVar (TV "c") `TypeArr` TypeVar (TV "b") `TypeArr` TypeVar (TV "a") `TypeArr` TypeVar (TV "c")))
    ("ap", Forall [TV "a", TV "b", TV "c"] (TypeVar (TV "a") `TypeArr` TypeVar (TV "b") `TypeArr` TypeVar (TV "c") `TypeArr` TypeVar (TV "a") `TypeArr` TypeVar (TV "b") `TypeArr` TypeVar (TV "a") `TypeArr` TypeVar (TV "c"))),
    ("print", Forall [TV "a"] (TypeVar (TV "a") `TypeArr` TypePCon (PCon "IO" [TypeCon "()"])))
  ]

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv $ Map.fromList primTypes

typeof :: TypeEnv -> Var -> Maybe Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  freshTyVar :: a -> Set.Set TypeVar

instance Substitutable Type where
  apply _ (TypeCon a) = TypeCon a
  apply s t@(TypeVar a) = Map.findWithDefault t a s
  apply s (t1 `TypeArr` t2) = apply s t1 `TypeArr` apply s t2
  apply s (TypePCon (PCon name args)) = TypePCon $ PCon name (apply s args)

  freshTyVar TypeCon {} = Set.empty
  freshTyVar (TypeVar a) = Set.singleton a
  freshTyVar (t1 `TypeArr` t2) = freshTyVar t1 `Set.union` freshTyVar t2
  freshTyVar (TypePCon (PCon _ args)) = freshTyVar args

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where
      s' = foldr Map.delete s as
  freshTyVar (Forall as t) = freshTyVar t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  freshTyVar = foldr (Set.union . freshTyVar) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  freshTyVar (TypeEnv env) = freshTyVar $ Map.elems env

nullSubst :: Subst
nullSubst = Map.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

unify :: Type -> Type -> Infer Subst
unify (l `TypeArr` r) (l' `TypeArr` r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s2 `compose` s1)
unify (TypeVar a) t = bind a t
unify t (TypeVar a) = bind a t
unify (TypeCon a) (TypeCon b) | a == b = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TypeVar -> Type -> Infer Subst
bind a t
  | t == TypeVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ Map.singleton a t

occursCheck :: Substitutable a => TypeVar -> a -> Bool
occursCheck a t = a `Set.member` freshTyVar t

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s {count = count s + 1}
  return $ TypeVar $ TV (letters !! count s)

instantiate :: Scheme -> Infer Type
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = Map.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall as t
  where
    as = Set.toList $ freshTyVar t `Set.difference` freshTyVar env

ops :: Binop -> Type
ops Add = typeInt `TypeArr` typeInt `TypeArr` typeInt
ops Mul = typeInt `TypeArr` typeInt `TypeArr` typeInt
ops Sub = typeInt `TypeArr` typeInt `TypeArr` typeInt
ops Eql = typeInt `TypeArr` typeInt `TypeArr` typeBool
ops Lss = typeInt `TypeArr` typeInt `TypeArr` typeBool
ops Gtr = typeInt `TypeArr` typeInt `TypeArr` typeBool

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s -> do
      t <- instantiate s
      return (nullSubst, t)

infer :: TypeEnv -> RExpr -> Infer (Subst, Type)
infer env ex = case ex of
  Var x -> lookupEnv env x
  Lam x e -> do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `TypeArr` t1)
  App e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3 <- unify (apply s2 t1) (TypeArr t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
  Let x e1 e2 -> do
    (s1, t1) <- infer env e1
    let env' = apply s1 env
        t' = generalize env' t1
    (s2, t2) <- infer (env' `extend` (x, t')) e2
    return (s2 `compose` s1, t2)
  If cond tr fl -> do
    tv <- fresh
    inferPrim env [cond, tr, fl] (typeBool `TypeArr` tv `TypeArr` tv `TypeArr` tv)
  Fix e1 -> do
    tv <- fresh
    inferPrim env [e1] ((tv `TypeArr` tv) `TypeArr` tv)
  Op op e1 e2 -> do
    inferPrim env [e1, e2] (ops op)
  Lit (LInt _) -> return (nullSubst, typeInt)
  Lit (LBool _) -> return (nullSubst, typeBool)

inferPrim :: TypeEnv -> [RExpr] -> Type -> Infer (Subst, Type)
inferPrim env l t = do
  tv <- fresh
  (s1, tf) <- foldM inferStep (nullSubst, id) l
  s2 <- unify (apply s1 (tf tv)) t
  return (s2 `compose` s1, apply s2 tv)
  where
    inferStep (s, tf) expr = do
      (s', t) <- infer (apply s env) expr
      return (s' `compose` s, tf . TypeArr t)

inferRExpr :: TypeEnv -> RExpr -> Either TypeError Scheme
inferRExpr env = runInfer . infer env

inferTop :: TypeEnv -> [(String, RExpr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex) : xs) = case inferRExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall ts body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TypeVar a) = [a]
    fv (TypeArr a b) = fv a ++ fv b
    fv (TypeCon _) = []
    fv (TypePCon (PCon _ args)) = concatMap fv args

    normtype (TypeArr a b) = TypeArr (normtype a) (normtype b)
    normtype (TypeCon a) = TypeCon a
    normtype (TypeVar a) =
      case lookup a ord of
        Just x -> TypeVar x
        Nothing -> error "type variable not in signature"
    normtype (TypePCon (PCon name args)) = TypePCon (PCon name (fmap normtype args))
