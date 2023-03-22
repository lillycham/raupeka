module Language.Raupeka.Compiler.Parser.Typing where

-- \| Contains parsers for type signatures and declarations.

import Control.Monad (void)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import Language.Raupeka.Compiler.Parser
import Language.Raupeka.Type.Checker
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

conid :: Parser String
conid = (:) <$> upperChar <*> many alphaNumChar

varid :: Parser String
varid = (:) <$> lowerChar <*> many alphaNumChar

tycon :: Parser Type
tycon = TypeCon <$> conid

tyvar :: Parser Type
tyvar = TypeVar . TV <$> varid

unit :: Parser Type
unit = TypeCon "()" <$ symbol "()"

anyType :: Parser Type
anyType = parameterized <|> specialType <|> tyvar <|> tycon

tylist :: Parser Type
tylist = do
  _ <- symbol "[" <* sc
  t <- anyType
  _ <- sc *> symbol "]" <* sc
  pure $ TypePCon $ PCon "List" [t]

-- | Parses a syntactically special type, e.g. () or the list constructor []
specialType :: Parser Type
specialType = unit <|> tylist

-- | Parses a parameterized type, e.g. Maybe a
parameterized :: Parser Type
parameterized = do
  n <- conid <* sc
  args <- some (tyvar <|> tycon <|> parameterized <|> specialType)
  pure $ TypePCon $ PCon n args

tysig :: Parser (Text, Scheme)
tysig = do
  name <- T.pack <$> identifier
  _ <- sc *> symbol ":" <* sc
  ty <- scheme
  pure (name, ty)

-- | carrow parses the "=>" symbol, which is used in constraints.
--   e.g. Num a => a -> a, which is syntactic sugar for ∀ a. Num a => a -> a
carrow :: Parser ()
carrow = void $ sc *> symbol "=>" <* sc

-- | Parses the forall quantifier, returning a list of type variables.
forallVA :: Parser [String]
forallVA = do
  _ <- symbol "∀" <|> symbol "forall"
  some (varid <* sc) <* symbol "." <* sc

forallVA' :: Parser [TypeVar]
forallVA' = map TV <$> forallVA

-- | rscheme parses a type scheme, where the "forall" quantifier is explicitly included.
--   rscheme ::= forall|∀ <vars> => <var> | <con> | <arrow>
rscheme :: Parser Scheme
rscheme = Forall <$> forallVA' <*> rtypeid'

-- | Parses an "implicit" type scheme, where the "forall" quantifier is optional.
ischeme :: Parser Scheme
ischeme = Forall <$> lookAhead allvars <*> rtypeid'

-- | Parses a type scheme, where the "forall" quantifier is optional.
scheme :: Parser Scheme
scheme = rscheme <|> ischeme

-- | Matches on "->" or "→"
arrow :: Parser ()
arrow = void $ sc *> (symbol "->" <|> symbol "→") <* sc

-- | Consumes components of a type signature until it reaches a type variable (as opposed to a type constructor)
stripcon :: Parser ()
stripcon = void $ many $ conid <* optional arrow

-- | Returns a list of all type variables in a type signature, not including any type constructors (i.e. Int, Bool, ...).
allvars :: Parser [TypeVar]
allvars =
  fmap TV . nub
    <$> ( optional stripcon
            *> ( some varid
                   <* optional arrow
                   <* optional stripcon
               )
        )

tyarr :: Parser Type
tyarr = do
  left <- rtypeid
  right <- optional $ arrow *> rtypeid'
  case right of
    Nothing -> pure left
    Just right' -> pure $ TypeArr left right'

rtypeid :: Parser Type
rtypeid = tycon <|> tyvar

rtypeid' :: Parser Type
rtypeid' = tyarr <|> rtypeid
