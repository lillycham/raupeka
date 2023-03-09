{-# LANGUAGE BangPatterns #-}
module Language.Raupeka.File where

{-| Module      : Language.Raupeka.File
    Description : Runs raupeka files.
    License     : BSD3
    Maintainer  : Lilly Cham
    Stability   : experimental -}

import Language.Raupeka.AST
import Language.Raupeka.Parser
import Language.Raupeka.Desugar
import Language.Raupeka.TypeCheck
import Language.Raupeka.SKICompiler

import Text.Megaparsec (errorBundlePretty)
import Control.DeepSeq (deepseq)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Text qualified as T
import Data.Map qualified as M

-- | Given a file path, run the file.
execFile :: FilePath -> IO ()
execFile path = do
  file <- T.readFile path
  let parsed = parseModule path file
  case parsed of
    Left err -> fail $ errorBundlePretty err
    Right exprs -> do
      runBindings exprs

-- | Given a list of bindings, run the main function.
runBindings :: [Binding] -> IO ()
runBindings bindings = do
  -- Compile all bindings
  let compiled = map (\(name, expr) -> (name, compile $ desugar expr)) bindings
  -- add bindings to env
  let env = M.union emptyREnv (M.fromList compiled)
  -- run main
  (runMainFn env) `deepseq` pure ()
  