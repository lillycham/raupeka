{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Raupeka.Type.Checker where

import Control.Monad.Except
import Control.Monad.State
import Data.List (nub)
import Data.Map qualified as Map
import Data.Monoid ()
import Data.Set qualified as Set
import Language.Raupeka.Compiler.AST
import Control.Applicative (Applicative(liftA2))

replicateS :: (Integral n, Semigroup s) => n -> s -> s
replicateS (signum -> -1) _ = error "replicateS not callable on negative numbers"
replicateS (liftA2 (||) (0 ==) (1 ==) -> True) x = x
replicateS n x = x <> replicateS (n - 1) x

type Var = String

newtype TypeVar = TV String
  deriving (Show, Eq, Ord)

arrToList :: Type -> [Type]
arrToList (TypeArr a b) = a : arrToList b
arrToList x = [x]

unvar :: TypeVar -> String
unvar (TV x) = x

-- | Kinds are the types of types.
data Kind
  = Kind'
  | Kind :^->: Kind
  deriving (Show, Eq)

instance Semigroup Kind where
  a <> b = a :^->: b

data Type
  = TypeVar TypeVar
  | TypeCon String
  | TypeArr Type Type
  | TypePCon String [Type]
  deriving (Show, Eq, Ord)

instance Semigroup Type where
  a <> b = a `TypeArr` b

infixr 9 `TypeArr`

data Scheme = Forall [TypeVar] Type
  deriving (Show, Eq, Ord)

schemetype :: Scheme -> Type
schemetype (Forall _ xs) = xs

hasUnused :: Scheme -> Bool
hasUnused (Forall vs t) = not $ null $ Set.fromList vs `Set.difference` ftv t

warnUnused :: Scheme -> Scheme
warnUnused s@(Forall vs _) =
  if hasUnused s
    then error $ "Unused type variables: " <> show vs
    else s

typeInt :: Type
typeInt = TypeCon "Int"

typeBool :: Type
typeBool = TypeCon "Bool"

typeList :: [Type] -> Type
typeList = TypePCon "List"

-- | Type Inference
newtype TypeEnv = TypeEnv (Map.Map Var Scheme)
  deriving (Semigroup, Monoid, Show)

extractEnv :: TypeEnv -> Map.Map Var Scheme
extractEnv (TypeEnv m) = m

newtype Unique = Unique {count :: Int}
  deriving (Eq, Ord, Show, Num)

type Infer = ExceptT TypeError (State Unique)

type Subst = Map.Map TypeVar Type

data TypeError
  = UnificationFail Type Type
  | InfiniteType TypeVar Type
  | UnboundVariable String
  | KindArityMismatch Type Kind Kind
  | KindPartiallyApplied Type Kind
  deriving (Show)

runInfer :: Infer (Subst, Type) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initUnique of
  Left err -> Left err
  Right res -> Right $ closeOver res

runInferK :: Infer Kind -> Either TypeError Kind
runInferK m = evalState (runExceptT m) initUnique

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
    ("print", Forall [TV "a"] (TypeVar (TV "a") `TypeArr` (TypeCon "()"))),
    -- Inbuilt type constructors
    ("List", Forall [TV "a"] $ TypePCon "List" [TypeVar $ TV "a"])
  ]

emptyTyenv :: TypeEnv
emptyTyenv = TypeEnv $ Map.fromList primTypes

typeof :: TypeEnv -> Var -> Maybe Scheme
typeof (TypeEnv env) name = Map.lookup name env

class Substitutable a where
  apply :: Subst -> a -> a
  ftv :: a -> Set.Set TypeVar

instance Substitutable Type where
  apply _ (TypeCon a) = TypeCon a
  apply s t@(TypeVar a) = Map.findWithDefault t a s
  apply s (t1 `TypeArr` t2) = apply s t1 `TypeArr` apply s t2
  apply s (TypePCon name args) = TypePCon name (apply s args)

  ftv TypeCon {} = Set.empty
  ftv (TypeVar a) = Set.singleton a
  ftv (t1 `TypeArr` t2) = ftv t1 `Set.union` ftv t2
  ftv (TypePCon _ args) = ftv args

instance Substitutable Scheme where
  apply s (Forall as t) = Forall as $ apply s' t
    where
      s' = foldr Map.delete s as
  ftv (Forall as t) = ftv t `Set.difference` Set.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv = foldr (Set.union . ftv) Set.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env
  ftv (TypeEnv env) = ftv $ Map.elems env

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
unify a@(TypePCon _ _) b@(TypePCon _ _) | a == b = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: TypeVar -> Type -> Infer Subst
bind a t
  | t == TypeVar a = return nullSubst
  | occursCheck a t = throwError $ InfiniteType a t
  | otherwise = return $ Map.singleton a t

occursCheck :: Substitutable a => TypeVar -> a -> Bool
occursCheck a t = a `Set.member` ftv t

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
    as = Set.toList $ ftv t `Set.difference` ftv env

ops :: Binop -> Type
ops Add = typeInt `TypeArr` typeInt `TypeArr` typeInt
ops Mul = typeInt `TypeArr` typeInt `TypeArr` typeInt
ops Sub = typeInt `TypeArr` typeInt `TypeArr` typeInt
ops Eql = (TypeVar (TV "a")) `TypeArr` ((TypeVar (TV "a")) `TypeArr` typeBool)
ops Lss = typeInt `TypeArr` typeInt `TypeArr` typeBool
ops Gtr = typeInt `TypeArr` typeInt `TypeArr` typeBool
ops Map
  = (TypeVar (TV "a")
  `TypeArr` TypeVar (TV "b"))
  `TypeArr` typeList [TypeVar (TV "a")]
  `TypeArr` typeList [TypeVar (TV "b")]

lookupEnv :: TypeEnv -> Var -> Infer (Subst, Type)
lookupEnv (TypeEnv env) x =
  case Map.lookup x env of
    Nothing -> throwError $ UnboundVariable (show x)
    Just s -> do
      t <- instantiate s
      return (nullSubst, t)

inferKind :: TypeEnv -> Type -> Infer Kind
inferKind _ (TypeCon _) = pure Kind'
inferKind env (TypePCon n a) = do
    tycon <- lookupEnv env n
    case tycon of
      (_, TypeCon _) -> error $ n <> " has a kind of *, but I expected a kind of * -> *."
      (_, TypePCon _ xs) -> pure $ case compare (length xs) $ length a of
        EQ -> Kind'
        GT -> replicateS (length xs - length a + 1) Kind'
        LT -> error $ "Expected " <> show (length xs - length a) <> " more arguments to " <> n
      (_, x) -> inferKind env x
inferKind env (a `TypeArr` b) = do
    k <- inferKind env a
    case k of
      Kind' -> inferKind env b
      _ -> throwError $ KindPartiallyApplied a k 
inferKind _ (TypeVar _) = pure Kind'

inferSchemeKind :: TypeEnv -> Scheme -> Infer Kind
inferSchemeKind env (Forall _ t) = inferKind env t

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
  Lit (LInt _) -> pure (nullSubst, typeInt)
  Lit (LBool _) -> pure (nullSubst, typeBool)
  Lit (LList xs) -> do
    mapM (infer env) xs >>= \xsty -> pure (nullSubst, typeList (snd <$> xsty))


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

inferRExprKind :: TypeEnv -> RExpr -> Either TypeError Kind
inferRExprKind env expr = do
  ty <- runInfer $ infer env expr
  runInferK $ inferSchemeKind env ty

inferTop :: TypeEnv -> [(String, RExpr)] -> Either TypeError TypeEnv
inferTop env [] = Right env
inferTop env ((name, ex) : xs) = case inferRExpr env ex of
  Left err -> Left err
  Right ty -> inferTop (extend env (name, ty)) xs

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (fmap snd ord) (normtype body)
  where
    ord = zip (nub $ fv body) (fmap TV letters)

    fv (TypeVar a) = [a]
    fv (TypeArr a b) = fv a ++ fv b
    fv (TypeCon _) = []
    fv (TypePCon _ args) = concatMap fv args

    normtype (TypeArr a b) = TypeArr (normtype a) (normtype b)
    normtype (TypeCon a) = TypeCon a
    normtype (TypeVar a) =
      case lookup a ord of
        Just x -> TypeVar x
        Nothing -> error "type variable not in signature"
    normtype (TypePCon name args) = TypePCon name (fmap normtype args)
