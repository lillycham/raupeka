module Main (main) where

import Language.Raupeka.Repl
import Language.Raupeka.File
import Options.Applicative

data Options = Options 
	{ optCommand :: Command }

data Command 
	= Repl
	| Run FilePath

commandParser :: Parser Command
commandParser = subparser
	( command "repl" (info (pure Repl) (progDesc "Start the REPL"))
	<> command "run" (info (Run <$> argument str (metavar "FILE")) (progDesc "Run a file")))
	-- default to repl
	<|> pure Repl

optsParser = info (Options <$> commandParser) (fullDesc <> progDesc "Raupeka interpreter")

main :: IO ()
main = do
	opts <- execParser optsParser
	case optCommand opts of
		Repl -> startRepl
		Run file -> execFile file
