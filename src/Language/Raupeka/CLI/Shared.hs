module Language.Raupeka.CLI.Shared (module Language.Raupeka.CLI.Shared) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Void (Void)
import Language.Raupeka.Pretty (rpretty)
import Language.Raupeka.Type.Checker
import Language.Raupeka.Type.Pretty ()
import Text.Megaparsec (ParseErrorBundle, TraversableStream, VisualStream, errorBundlePretty)

handleTypeErrors :: Either TypeError a -> (String -> b) -> (a -> b) -> b
handleTypeErrors (Left err) c _ = c $ "type inference failed:\n  " <> rpretty err
handleTypeErrors (Right a) _ f = f a

handleParseErrors ::
  ( VisualStream s,
    TraversableStream s
  ) =>
  Either (ParseErrorBundle s Void) a ->
  (String -> b) ->
  (a -> b) ->
  b
handleParseErrors (Left err) c _ = c $ "parse error:\n" <> errorBundlePretty err
handleParseErrors (Right a) _ f = f a

printParseErrors :: (VisualStream s, TraversableStream s, MonadIO m) => Either (ParseErrorBundle s Void) a -> m (Maybe a)
printParseErrors (Left err) = liftIO $ putStrLn ("parse error:\n" <> errorBundlePretty err) >> pure Nothing
printParseErrors (Right a) = pure $ Just a
