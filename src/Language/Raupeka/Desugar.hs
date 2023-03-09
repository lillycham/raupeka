module Language.Raupeka.Desugar where

import Language.Raupeka.AST
import Data.Text ()

{-|
Module      : Language.Raupeka.Desugar
Description : Desugars raupeka stx sugar into function application.
Copyright   : (c) Lilly Cham, 2023
License     : BSD3
Stability   : experimental
-}

desugar :: RExpr -> RExpr
desugar (App fn arg)    = App (desugar fn) (desugar arg)
desugar (Lam name body) = Lam name (desugar body)
desugar (Let x e body)  = App (Lam x (desugar body)) (desugar e)
desugar (If cond t f)   = foldl App (Var "$if") args
  where args = desugar <$> [cond, t, f]
desugar (Fix e)         = App (Var "$fix") (desugar e)
desugar (Op op lhs rhs) = foldl App (Var n) args
  where args = desugar <$> [lhs, rhs]
        n    = case op of
                Add -> "$add"
                Sub -> "$sub"
                Mul -> "$mul"
                Eql -> "$eql"
                Gtr -> "$gtr"
                Lss -> "$lss"
                Gte -> "$gte"
                Lse -> "$lse"
                Cmp -> "$compose"
desugar e               = e
