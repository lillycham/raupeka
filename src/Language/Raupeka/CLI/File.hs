module Language.Raupeka.CLI.File (execFile, runBindings, compileFileHs) where

-- \| Module      : Language.Raupeka.CLI.File
--    Description : Runs raupeka files.
--    License     : BSD3
--    Maintainer  : Lilly Cham
--    Stability   : experimental

import Control.DeepSeq (deepseq)
import Data.Bifunctor (second)
import Data.Map qualified as M
import Data.Text.IO qualified as T
import Language.Raupeka.CLI.Shared
import Language.Raupeka.Compiler.Desugar
import Language.Raupeka.Compiler.Parser
import Language.Raupeka.Compiler.SKI

-- | Given a file path, run the file.
execFile :: FilePath -> IO ()
execFile path = do
  file <- T.readFile path
  handleParseErrors (parseModule path file) fail \es -> do
    let compiled = map (\(name, expr) -> (name, compile $ desugar expr)) es
    let env = M.union emptyREnv (M.fromList compiled)
    case runMainFn env of
      Nothing -> putStrLn "No main function found."
      Just a -> print a

prelude :: String
prelude = "import Data.Function (fix)\n"

compileFileHs :: FilePath -> FilePath -> IO ()
compileFileHs path out = do
  file <- T.readFile path
  handleParseErrors (parseModule path file) fail \es -> do
    let compiled = map (\(name, expr) -> (name, compile $ desugar expr)) es
    let haskBinds = map (second toHs) compiled
    let hask = unlines $ map (\(name, expr) -> name <> " = " <> expr) haskBinds
    let hask' = prelude ++ hask
    writeFile out hask'

-- | Given a list of bindings, run the main function.
runBindings :: [Binding] -> IO ()
runBindings bindings = do
  -- Compile all bindings
  let compiled = map (\(name, expr) -> (name, compile $ desugar expr)) bindings
  -- add bindings to env
  let env = M.union emptyREnv (M.fromList compiled)
  -- run main
  case runMainFn env of
    Nothing -> fail "No main function found."
    Just e -> e `deepseq` pure ()
