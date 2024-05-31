{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Raupeka.CLI.Repl (startRepl) where

import Control.Exception (catch)
import Control.Monad (when)
import Control.Monad.State (MonadState, StateT, get, put, runStateT)
import Control.Monad.Trans (MonadIO (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Raupeka.CLI.Shared
import Language.Raupeka.Compiler.Desugar
import Language.Raupeka.Compiler.Parser
import Language.Raupeka.Compiler.SKI
import Language.Raupeka.Pretty (rpretty)
import Language.Raupeka.Type.Checker
import Language.Raupeka.Type.Pretty ()
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.IO.Error (isEOFError)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT)

data ReplReturn
  = ShouldQuit
  | ShouldContinue
  deriving (Eq, Show)

data ReplState = ReplState
  { langEnv :: REnv,
    typeEnv :: TypeEnv,
    rCont :: ReplReturn
  }

emptyReplState :: ReplState
emptyReplState =
  ReplState
    { langEnv = emptyREnv,
      typeEnv = emptyTyenv,
      rCont = ShouldContinue
    }

-- | Repl Monad
--   The Repl monad encapsulates the state of the Repl, and provides
--   a way to handle errors and quit the Repl.
newtype Repl a = Repl (StateT ReplState IO a)
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadState ReplState, MonadFail)
    via (StateT ReplState IO)

runRepl :: Repl a -> ReplState -> IO (a, ReplState)
runRepl (Repl m) = runStateT m

data REPLCommand
  = Quit
  | ShowEnv
  | Eval Text
  | Typecheck Text
  | Desugar Text
  | Compile Text
  | Unknown Text
  | Kind Text
  deriving (Eq, Show, Read)

cmdPrefix :: Text -> REPLCommand
cmdPrefix (T.stripPrefix ":q" -> Just _) = Quit
cmdPrefix (T.stripPrefix ":a " -> Just e) = Desugar e
cmdPrefix (T.stripPrefix ":s " -> Just e) = Compile e
cmdPrefix (T.stripPrefix ":t " -> Just e) = Typecheck e
cmdPrefix (T.stripPrefix ":k " -> Just e) = Kind e
cmdPrefix (T.stripPrefix ":e" -> Just _) = ShowEnv
cmdPrefix e@(T.stripPrefix ":" -> Just _) = Unknown e
cmdPrefix e = Eval e

repl' :: Repl ()
repl' =
  get >>= \renv ->
    let env = langEnv renv
        tyEnv = typeEnv renv
     in prompt
          >>= ( \case
                  -- Exit the repl
                  Quit -> put renv {rCont = ShouldQuit}
                  -- Desugar the input, then print the result.
                  Unknown i -> output ("Unknown command: " <> i)
                  ShowEnv -> (liftIO . printMap) env >> (liftIO . printMap . extractEnv) tyEnv
                  Desugar i -> handleParseErrors (desugar <$> parseExprs file i) handle outputShow
                  -- Desugar and compile to SKI, then print the result.
                  Compile i -> handleParseErrors (parseExprs file i) handle (outputShow . compile . desugar)
                  -- Typecheck the input, then print the result.
                  Typecheck i ->
                    handleParseErrors (parseExprs file i) handle \es ->
                      handleTypeErrors (inferRExpr tyEnv es) handle (output' . rpretty)
                  Kind i -> handleParseErrors (parseExprs file i) handle \es ->
                    handleTypeErrors (inferRExprKind tyEnv es) handle (output' . rpretty)
                  -- Default case: evaluate the input and print the result
                  Eval i ->
                    handleParseErrors (parseExprs file i) handle \es -> outputShow $ eval env es
                  . cmdPrefix
              )
  where
    file = "<repl>:"
    handle = output'

startRepl :: IO ()
startRepl = do
  _ <- installHandler sigINT (Catch (putStrLn "\b\b\nInterrupted." >> repl)) Nothing
  putStrLn "Welcome to Raupeka!"
  putStrLn "Type :q to quit."
  putStrLn "Type :a to print the AST."
  putStrLn "Type :s to compile to SKI."
  hSetBuffering stdout NoBuffering
  repl

repl :: IO ()
repl =
  ( runRepl repl' emptyReplState >>= \case
      (_, rCont -> ShouldQuit) -> exitSuccess
      (_, rCont -> ShouldContinue) -> liftIO repl
      _ -> error "panic: an unrecoverable state was reached. this is likely a compiler bug."
  )
    `catch` \e ->
      when (isEOFError e) exitSuccess

prompt :: Repl Text
prompt = liftIO (putStr "> " >> T.getLine)

output :: Text -> Repl ()
output = liftIO . T.putStrLn

output' :: String -> Repl ()
output' = liftIO . putStrLn

outputShow :: Show a => a -> Repl ()
outputShow = output . T.pack . show
