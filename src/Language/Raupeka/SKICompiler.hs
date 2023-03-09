{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Language.Raupeka.SKICompiler {- (eval, runEval, REnv, emptyREnv, CExpr) -} where

import Language.Raupeka.Types (Name)
import Language.Raupeka.AST
import Language.Raupeka.Desugar

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Function (fix)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

import System.IO.Unsafe (unsafePerformIO)

{-|
Module      : Language.Raupeka.SKICompiler
Description : Compiles desugared Raupeka into an enhanced SKI Combinator Calculus, or Raupeka core.
Copyright   : (c) Lilly Cham, 2023
License     : BSD3
Stability   : experimental
-}

data CExpr
  = CVar Name
  | CApp CExpr CExpr
  | CLam (CExpr -> CExpr)
  | CBool Bool
  | CInt Integer
  | CStr Text
  | CList [CExpr]
  | CVoid -- ^ The unit type
  deriving (Generic, NFData)

instance Show CExpr where
  show (CVar n) = n
  show (CApp fun arg) = "(" ++ show fun ++ " " ++ show arg ++ ")"
  show (CLam _) = "<function>"
  show (CBool b) = show b
  show (CInt i) = show i
  show (CStr s) = show s
  show (CList xs) = show xs
  show (CVoid) = "#v"

compile :: RExpr -> CExpr
compile (Var n) = CVar n
compile (App fun arg) = CApp (compile fun) (compile arg)
compile (Lam x body) = abstract x (compile body)
compile (Lit (LInt k)) = CInt k
compile (Lit (LBool k)) = CBool k
compile (Lit (LStr k)) = CStr k
compile (Lit (LList k)) = CList (compile <$> k)

abstract :: Name -> CExpr -> CExpr
abstract x (CApp fun arg) = combS (abstract x fun) (abstract x arg)
abstract x (CVar n) | x == n = combI
abstract _ k = combK k

combS :: CExpr -> CExpr -> CExpr
combS f = CApp (CApp (CVar "$s") f)

combK :: CExpr -> CExpr
combK = CApp (CVar "$k")

combI :: CExpr
combI = CVar "$i"

-- Inbuilts
capply :: CExpr -> CExpr -> CExpr
capply (CLam f) x = f x

infixl 0 !
(!) :: CExpr -> CExpr -> CExpr
(CLam f) ! x = f x

prims :: [(Name, CExpr)]
prims =
  [ ("$i", CLam $ id)
  , ("$k", CLam $ \x -> CLam $ \_ -> x)
  , ("$s", CLam $ \f -> CLam $ \g -> CLam $ \x -> f!x!(g!x))
  , ("$if", CLam $ \(CBool cond) -> CLam $ \tr -> CLam $ \fl -> if cond then tr else fl)
  , ("$fix", CLam $ \(CLam f) -> fix f)
  , ("$compose", CLam $ \(CLam f) -> CLam $ \(CLam g) -> CLam $ \x -> f(g x))
  , ("$add", arith (+))
  , ("$sub", arith (-))
  , ("$mul", arith (*))
  , ("$eql", logical (==))
  , ("$gtr", logical (>))
  , ("$lss", logical (<))
  , ("$gte", logical (>=))
  , ("$lse", logical (<=))
  -- IO
  , ("$print", CLam $ \case (CStr i) -> unsafePerformIO $ T.putStrLn i >> pure CVoid
                            i -> unsafePerformIO $ print i >> pure CVoid)]
  
arith :: (Integer -> Integer -> Integer) -> CExpr
arith op = CLam $ \(CInt a) -> CLam $ \(CInt b) -> CInt (op a b)

logical :: (Integer -> Integer -> Bool) -> CExpr
logical op = CLam $ \(CInt a) -> 
             CLam $ \(CInt b) -> if op a b then true else false

true, false :: CExpr
true = CBool True
false = CBool False

-- Env
type REnv = M.Map Name CExpr

emptyREnv :: REnv
emptyREnv = M.fromList prims

link :: REnv -> CExpr -> CExpr
link bs (CApp fun arg) = link bs fun `capply` link bs arg
link bs (CVar n) = fromJust (M.lookup n bs)
link _ e = e

eval :: REnv -> RExpr -> CExpr
eval env = link env . compile . desugar

runEval :: REnv -> Name -> RExpr -> (CExpr, REnv)
runEval env nm ex =
  let res = eval env ex in
  (res, M.insert nm res env)

runMainFn :: REnv -> Maybe CExpr
runMainFn env = M.lookup "main" env >>= \expr -> Just (link env expr)


