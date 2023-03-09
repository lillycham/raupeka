{-# LANGUAGE ViewPatterns #-}
module Language.Raupeka.Repl (startRepl) where

import Language.Raupeka.Parser
import Language.Raupeka.SKICompiler
import Language.Raupeka.Desugar
import Text.Megaparsec
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering))
import System.IO.Error (isEOFError)
import System.Exit (exitSuccess)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT)
import Control.Exception (catch)

startRepl :: IO ()
startRepl = putStrLn "Welcome to Raupeka!"
  >> putStrLn "Type :q to quit."
  >> putStrLn "Type :d to desugar."
  >> putStrLn "Type :s to compile to SKI."
  >> hSetBuffering stdout NoBuffering
  >> repl

repl :: IO ()
repl = installHandler sigINT (Catch (putStrLn "" >> repl)) Nothing >>
  (repl' emptyREnv) `catch` \case
    (isEOFError -> True) -> exitSuccess
    e -> putStrLn $ "Error: " ++ show e

repl' :: REnv -> IO ()
repl' env = 
  putStr "> " >> T.getLine >>= \input ->
    -- if input has a command, strip it and run the command
    -- otherwise, evaluate the input
    case (stripCmd input, T.take 2 input) of
      (_,":q") -> putStrLn "Bye!"
      (i ,":d") -> do
        putStrLn "Desugared:"
        let desugared = desugar <$> parseExprs file i
        case desugared of
          Left err -> putStrLn $ errorBundlePretty err
          Right exprs -> print $ exprs
        repl' env
      (i, ":s") -> do
        putStrLn "SKI:"
        let ski = compile <$> desugar <$> parseExprs file i
        case ski of
          Left err -> putStrLn $ errorBundlePretty err
          Right ski' -> print $ ski'
        repl' env
{-       (i, ":t") -> do
        let evaled = eval env <$> parseExprs file i
        case evaled of
          Left err -> fail $ errorBundlePretty err
          Right evaled' -> print $ typeof evaled'
        repl' env -}
      (i, _) -> do
        let evaled = eval env <$> parseExprs file i
        case evaled of
          Left err -> putStrLn $ errorBundlePretty err
          Right evaled' -> print $ evaled'
        repl' env
  where file = "<repl>:"

stripCmd :: Text -> Text
stripCmd (T.stripPrefix ":d " -> Just t) = t
stripCmd (T.stripPrefix ":s " -> Just t) = t
stripCmd (T.stripPrefix ":t " -> Just t) = t
stripCmd t = t