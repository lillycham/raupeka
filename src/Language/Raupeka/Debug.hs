module Language.Raupeka.Debug where

import Language.Raupeka.AST
import Language.Raupeka.Types
import Language.Raupeka.Parser
import Language.Raupeka.SKICompiler
import Language.Raupeka.Desugar
import Text.Megaparsec
import Data.Text (Text)

evalToNormalForm :: Text -> IO ()
evalToNormalForm input = do
  let env = emptyREnv
  let res = runParser pExprs "<debug>" input
  case res of
    Left err -> fail $ errorBundlePretty err
    Right exprs -> do
      let normalForm = eval env $ exprs
      print normalForm

compileToSKI :: Text -> IO ()
compileToSKI input = do
  let res = runParser pExprs "<debug>" input
  case res of
    Left err -> fail $ errorBundlePretty err
    Right exprs -> do
      let ski = compile $ desugar $ exprs
      print ski

desugarProgram :: Text -> IO ()
desugarProgram input = do
  let res = runParser pExprs "<debug>" input
  case res of
    Left err -> fail $ errorBundlePretty err
    Right exprs -> do
      let desugared = desugar exprs
      print desugared