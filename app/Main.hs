module Main (main) where

import Language.Raupeka.CLI.File
import Language.Raupeka.CLI.Repl
import Options.Applicative

newtype Options = Options
  {optCommand :: Command}

data Command
  = Repl
  | Run FilePath
  | Compile FilePath FilePath

commandParser :: Parser Command
commandParser =
  subparser
    ( command "repl" (info (pure Repl) (progDesc "Start the REPL"))
        <> command "run" (info (Run <$> argument str (metavar "FILE")) (progDesc "Run a file"))
        <> command
          "compile"
          ( info
              ( Compile
                  <$> argument str (metavar "FILE")
                  <*> argument str (metavar "OUT" <> value "out.hs")
              )
              (progDesc "Compile a file to Haskell")
          )
    )
    -- default to repl
    <|> pure Repl

optsParser :: ParserInfo Options
optsParser = info (Options <$> commandParser) (fullDesc <> progDesc "Raupeka interpreter")

main :: IO ()
main = do
  opts <- execParser optsParser
  case optCommand opts of
    Repl -> startRepl
    Run file -> execFile file
    Compile file out -> compileFileHs file out
