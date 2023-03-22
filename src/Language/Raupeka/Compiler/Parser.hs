module Language.Raupeka.Compiler.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Char (GeneralCategory (..))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Language.Raupeka.Compiler.AST
import Language.Raupeka.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

-- Lexing
reservedNames :: [Text]
reservedNames =
  [ "let",
    "in",
    "fix",
    "s",
    "k",
    "i",
    "true",
    "false",
    "fix",
    "if",
    "then",
    "else"
  ]

reservedOps :: [Text]
reservedOps =
  [ "->",
    "→",
    ":",
    "\\",
    "λ",
    "+",
    "*",
    "-",
    "="
  ]

rws :: [Text]
rws = reservedNames <> reservedOps

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

scn :: Parser ()
scn = L.space space1 lineComment blockComment

-- space consumer
-- comments as in haskell:
-- - `--`
-- - `{- -}`
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- charLiteral :: Parser Char
-- charLiteral = between (char '\'') (char '\'') L.charLiteral

-- \d
integer :: Parser Integer
integer = lexeme L.decimal

-- -?\d
signedInteger :: Parser Integer
signedInteger = L.signed (pure ()) integer

-- variables
pVar :: Parser RExpr
-- Don't allow reserved names as variables
pVar = Var <$> identifier

pString :: Parser RExpr
pString = (Lit . LStr) . T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pList :: Parser RExpr
pList = Lit . LList <$> between (symbol "[") (symbol "]") (exprs `sepBy` symbol ",")

identifierChar :: Parser Char
identifierChar = alphaNumChar <|> char '_' <|> char '\'' <|> char '-' <|> charCategory MathSymbol

-- variable parser
-- variable must start with a letter
identifier :: Parser Name
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many identifierChar
    check x = if T.pack x `elem` rws then fail $ show x <> " cannot be an identifier" else pure x

pInteger :: Parser RExpr
pInteger = Lit . LInt <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser RExpr
pTerm =
  choice
    [ parens exprs,
      lambdaExpr,
      letIn,
      ifThenElse,
      pInteger,
      pList,
      pString,
      pVar
      -- , section
      -- , leftSection
      -- , rightSection
    ]

-- * Operators

binary :: Text -> (RExpr -> RExpr -> RExpr) -> Operator Parser RExpr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (RExpr -> RExpr) -> Operator Parser RExpr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

operatorTable :: [[Operator Parser RExpr]]
operatorTable =
  [ [binary " " App],
    [binary "." (Op Cmp)],
    [binary "+" (Op Add)],
    [binary "*" (Op Mul)],
    [binary "-" (Op Sub)],
    -- , [ binary "/"  (Op Div) ]
    [binary "<$>" (Op Map)],
    [binary "==" (Op Eql)],
    [binary ">" (Op Gtr)],
    [binary "<" (Op Lss)],
    [binary ">=" (Op Gte)],
    [binary "<=" (Op Lse)],
    [binary "$" App]
  ]

binOps :: [Text]
binOps = ["+", "-", "*", "/", ".", ">", "<", ">=", "<=", "==", "<$>"]

-- * Operator Sections

-- section = parens $ do
--   op <- choice binOps
--   pure $ Op $ case op of
--     "+" -> Add
--     "-" -> Sub
--     "*" -> Mul
--     "/" -> Div
--     "." -> Cmp
--     ">" -> Gtr
--     "<" -> Lss
--     ">=" -> Gte
--     "<=" -> Lse
--     "==" -> Eql
--     "<$>" -> Map
--     _ -> fail "impossible"

-- leftSection = parens $ do
--   op <- choice binOps
--   e <- exprs
--   pure $ Op $ case op of
--     "+" -> Add
--     "-" -> Sub
--     "*" -> Mul
--     "/" -> Div
--     "." -> Cmp
--     ">" -> Gtr
--     "<" -> Lss
--     ">=" -> Gte
--     "<=" -> Lse
--     "==" -> Eql
--     "<$>" -> Map
--     _ -> fail "impossible"

-- rightSection = parens $ do
--   e <- exprs
--   op <- choice binOps
--   pure $ Op $ case op of
--     "+" -> Add
--     "-" -> Sub
--     "*" -> Mul
--     "/" -> Div
--     "." -> Cmp
--     ">" -> Gtr
--     "<" -> Lss
--     ">=" -> Gte
--     "<=" -> Lse
--     "==" -> Eql
--     "<$>" -> Map
--     _ -> error "impossible"

-- * Terms and Expressions

term :: Parser RExpr
term = makeExprParser pTerm operatorTable

-- If multiple terms are given, they are applied to each other from left to right.
exprs :: Parser RExpr
-- don't allow "let", or "in" as a variable name
-- an expression also ends when a ; is encountered
exprs = foldl1 App <$> some term <* optional (symbol ";")

-- parser for lambda expr
lambdaExpr :: Parser RExpr
lambdaExpr = do
  _ <- symbol "\\" <|> symbol "λ"
  args <- some identifier
  _ <- symbol "->"
  body <- exprs
  pure $ foldr Lam body args

-- let expr parser
-- let name = expr in expr'
letIn :: Parser RExpr
letIn = do
  _ <- symbol "let"
  name <- identifier
  _ <- symbol "="
  expr <- exprs
  _ <- symbol "in"
  Let name expr <$> exprs

type Binding = (Name, RExpr)

-- parser for boolean literals
boolean :: Parser RExpr
boolean =
  choice
    [ symbol "true" $> Lit (LBool True),
      symbol "false" $> Lit (LBool False)
    ]

-- parser for if then else expr
ifThenElse :: Parser RExpr
ifThenElse =
  If
    <$> (symbol "if" *> term)
    <*> (symbol "then" *> term)
    <*> (symbol "else" *> term)

-- parser for top level declarations
-- form is `let name args... = body`
letDecl :: Parser Binding
letDecl = do
  _ <- symbol "let"
  name <- identifier
  args <- many identifier
  _ <- symbol "="
  body <- exprs
  pure (name, foldr Lam body args)

letDecls :: Parser [Binding]
letDecls = some letDecl

data RModule = RModule Text [Binding]
  deriving (Eq, Show, Ord)

raupekaModule :: Parser [Binding]
-- A module is a list of top level declarations
raupekaModule = letDecls

-- Why does this only capture the first declaration?

-- moduleDecl :: Parser Text
-- moduleDecl = do
--   symbol "module"
--   name <- identifier
--   pure $ T.pack name

-- getModuleName :: RModule -> Text
-- getModuleName (RModule name _) = name

-- getModuleDecls :: RModule -> [Binding]
-- getModuleDecls (RModule _ decls) = decls

-- | Parse a module
-- parseTopLevel :: Parser RModule
-- parseTopLevel = RModule <$> (moduleDecl <?> "Main") <*> (some letDecl)

-- -- get bindings from a module
-- getBindings :: RModule -> [Binding]
-- getBindings (RModule _ decls) = decls

-- Import statements
-- import "file.rpka"
-- pImport :: Parser Text
-- pImport = do
--   symbol "import"
--   file <- between (char '"') (char '"') (manyTill L.charLiteral (char '"'))
--   pure $ T.pack file

-- Import haskell functions (i.e. FFI)
-- foreign import "Module.function"
-- pForeignImport :: Parser Text
-- pForeignImport = do
--   symbol "foreign"
--   symbol "import"
--   file <- between (char '"') (char '"') (manyTill L.charLiteral (char '"'))
--   pure $ T.pack file

parseExprs :: FilePath -> Text -> Either (ParseErrorBundle Text Void) RExpr
parseExprs = runParser exprs

parseModule :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Binding]
parseModule = runParser letDecls
