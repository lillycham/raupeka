{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Raupeka.Compiler.SKI (compile, toHs, link, eval, runEval, REnv, emptyREnv, CExpr (..), runMainFn, printMap) where

import Control.DeepSeq (NFData)
import Data.Map ((\\))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.Generics (Generic)
import Language.Raupeka.Compiler.AST
import Language.Raupeka.Compiler.Desugar
import Language.Raupeka.Types (Name)
import System.IO.Unsafe (unsafePerformIO)

-- |
-- Module      : Language.Raupeka.SKI
-- Description : Compiles desugared Raupeka into an enhanced SKI Combinator Calculus, or Raupeka core.
-- Copyright   : (c) Lilly Cham, 2023
-- License     : BSD3
-- Stability   : experimental

-- * Data types

-- | The ADT representing expressions in the combinator calculus
data CExpr
  = CVar Name
  | CApp CExpr CExpr
  | CLam (CExpr -> CExpr)
  | CBool Bool
  | CInt Integer
  | CStr Text
  | CList [CExpr]
  | CTuple [CExpr]
  deriving (Generic, NFData)

instance Show CExpr where
  show (CVar n) = n
  show (CApp fun arg) = "(" ++ show fun ++ " " ++ show arg ++ ")"
  show (CLam _) = "<function>"
  show (CBool b) = show b
  show (CInt i) = show i
  show (CStr s) = show s
  show (CList xs) = show xs
  show (CTuple xs) = "(" ++ unwords (show <$> xs) ++ ")"

-- * Compilation

-- | Compile a RExpr into a CExpr
compile :: RExpr -> CExpr
compile (Var n) = CVar n
compile (App fun arg) = CApp (compile fun) (compile arg)
compile (Lam x body) = abstract x (compile body)
compile (Lit (LInt k)) = CInt k
compile (Lit (LBool k)) = CBool k
compile (Lit (LStr k)) = CStr k
compile (Lit (LList k)) = CList (compile <$> k)
compile _ = error "panic: impossible state reached in compilation step [ast -> ski], file a bug report!"

-- | Compile a CExpr into the SKI combinator calculus, eliminating any free variables.
abstract :: Name -> CExpr -> CExpr
abstract x (CApp fun arg) = combS (abstract x fun) (abstract x arg)
abstract x (CVar n) | x == n = combI
abstract _ k = combK k

-- * Builtins

-- | The Starling combinator
combS :: CExpr -> CExpr -> CExpr
combS f = CApp (CApp (CVar "ap") f)

-- | The Kestrel, or const combinator
combK :: CExpr -> CExpr
combK = CApp (CVar "const")

-- | The I, or identity combinator
combI :: CExpr
combI = CVar "id"

-- | Apply a Lambda
capply :: CExpr -> CExpr -> CExpr
capply (CLam f) = f

infixl 0 !

-- | Infix form of capply.
(!) :: CExpr -> CExpr -> CExpr
(CLam f) ! x = f x

-- | Builtin operations
prims :: [(Name, CExpr)]
prims =
  -- Combinators
  [ ("id", CLam id),
    ("const", CLam $ \x -> CLam $ const x),
    ("ap", CLam $ \f -> CLam $ \g -> CLam $ \x -> f ! x ! (g ! x)),
    --  , ("fix", CLam \f -> fix f)
    ("iff", CLam $ \(CBool cond) -> CLam $ \tr -> CLam $ \fl -> if cond then tr else fl),
    ("compose", CLam $ \(CLam f) -> CLam $ \(CLam g) -> CLam $ \x -> f (g x)),
    ("add", arith (+)),
    ("sub", arith (-)),
    ("mul", arith (*)),
    ("$eql", logical (==)),
    ("$gtr", logical (>)),
    ("$lss", logical (<)),
    ("$gte", logical (>=)),
    ("$lse", logical (<=)),
    -- List ops
    ("head", CLam \(CList l) -> head l),
    ("tail", CLam \(CList l) -> CList $ tail l),
    ("map", CLam \(CLam f) -> CLam \(CList l) -> CList $ fmap f l),
    -- IO
    ( "print",
      CLam $ \case
        (CStr i) -> unsafePerformIO $ T.putStrLn i >> pure unit
        i -> unsafePerformIO $ print i >> pure unit
    )
  ]

unit :: CExpr
unit = CTuple []

-- | Performs arithmetic operations of type Int -> Int -> Int
arith :: (Integer -> Integer -> Integer) -> CExpr
arith op = CLam $ \(CInt a) -> CLam $ \(CInt b) -> CInt (op a b)

-- | Performs logical operations on integers returning bools
logical :: (Integer -> Integer -> Bool) -> CExpr
logical op = CLam $ \(CInt a) ->
  CLam $ \(CInt b) -> if op a b then true else false

-- | True and false.
true, false :: CExpr
true = CBool True
false = CBool False

-- * Environment

-- | Environment type
type REnv = M.Map Name CExpr

-- | Instantiate a new environment
emptyREnv :: REnv
emptyREnv = M.fromList prims

-- | Print a map
printMap :: (Show a, Show b) => M.Map a b -> IO ()
printMap = mapM_ print . M.toList

-- * Evaluation

-- | Get values from env
link :: REnv -> CExpr -> CExpr
link bs (CApp fun arg) = link bs fun `capply` link bs arg
link bs (CVar n) = fromMaybe (error "Value not in environment.") (M.lookup n bs)
link _ e = e

-- | Evaluate an expression with a given env
eval :: REnv -> RExpr -> CExpr
eval env = link env . compile . desugar

-- | Run a computation, storing the result in the env
runEval :: REnv -> Name -> RExpr -> (CExpr, REnv)
runEval env name ex =
  let res = eval env ex
   in (res, M.insert name res env)

-- | Run a main function in a given env
runMainFn :: REnv -> Maybe CExpr
runMainFn env = M.lookup "main" env >>= \expr -> Just (link env expr)

-- * Haskell backend

-- | Convert an environment into a haskell program
envToHs :: REnv -> String
envToHs env = unlines $ (\(n, e) -> n ++ " = " ++ toHs e) <$> M.toList (env \\ emptyREnv)

-- | Convert a CExpr as a haskell expression
toHs :: CExpr -> String
toHs (CVar "id") = "id"
toHs (CVar "const") = "const"
toHs (CVar "ap") = "(\\f -> \\g -> \\x -> f x (g x))"
toHs (CVar "iff") = "(\\cond -> \\tr -> \\fl -> if cond then tr else fl)"
toHs (CVar "fix") = "(\\f -> fix f)"
toHs (CVar "compose") = "(\\f -> \\g -> \\x -> f (g x))"
toHs (CVar "add") = "(\\a -> \\b -> a + b)"
toHs (CVar "sub") = "(\\a -> \\b -> a - b)"
toHs (CVar "mul") = "(\\a -> \\b -> a * b)"
toHs (CVar "$eql") = "(\\a -> \\b -> a == b)"
toHs (CVar "$gtr") = "(\\a -> \\b -> a > b)"
toHs (CVar "$lss") = "(\\a -> \\b -> a < b)"
toHs (CVar "$gte") = "(\\a -> \\b -> a >= b)"
toHs (CVar "$lse") = "(\\a -> \\b -> a <= b)"
toHs (CVar "print") = "(\\x -> print x)"
toHs (CVar n) = n
toHs (CApp fun arg) = "(" ++ toHs fun ++ " " ++ toHs arg ++ ")"
toHs (CBool b) = show b
toHs (CInt i) = show i
toHs (CStr s) = show s
toHs _ = error "toHs: not implemented"
