
module Language.Raupeka.Parser where

import Language.Raupeka.AST
import Language.Raupeka.Types

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Monad.Combinators.Expr
import Debug.Trace

type Parser = Parsec Void Text

-- Lexing
reservedNames :: [Text]
reservedNames = [ "let"
                , "in"
                , "fix"
                , "rec"
                , "if"
                , "then"
                , "else" ]

reservedOps :: [Text]
reservedOps = [ "->"
              , "→"
              , ":"
              , "\\"
              , "λ"
              , "+"
              , "*"
              , "-"
              , "=" ]

rws :: [Text]
rws = reservedNames <> reservedOps

-- space consumer
-- comments as in haskell:
-- - `--`
-- - `{- -}`
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  (L.skipBlockComment "{-" "-}")

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
pString = Lit <$> LStr <$> T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

pList :: Parser RExpr
pList = Lit <$> LList <$> between (symbol "[") (symbol "]") (pExprs `sepBy` symbol ",")

-- variable parser
-- variable must start with a letter
identifier :: Parser Name
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if (T.pack x) `elem` rws then fail $ show x <> " cannot be an identifier" else pure x

pInteger :: Parser RExpr
pInteger = Lit <$> LInt <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser RExpr
pTerm = choice
  [ parens pExprs
  , pLambda
  , pLetExpr
  , ifThenElse
  , pInteger
  , pList
  , pString
  , pVar
  ]

binary :: Text -> (RExpr -> RExpr -> RExpr) -> Operator Parser RExpr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (RExpr -> RExpr) -> Operator Parser RExpr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

operatorTable :: [[Operator Parser RExpr]]
operatorTable = 
  [ [ binary "$" App ]
  , [ binary " " App ]
  , [ binary "." (Op Cmp) ]
  , [ binary "+"  (Op Add) ]
  , [ binary "*"  (Op Mul) ]
  , [ binary "-"  (Op Sub)]
  --, [ binary "/"  (Op Div) ]
  , [ binary "==" (Op Eql) ]
  , [ binary ">"  (Op Gtr) ]
  , [ binary "<"  (Op Lss) ]
  , [ binary ">=" (Op Gte) ]
  , [ binary "<=" (Op Lse) ]
  , [ prefix "print" (App (Var "$print")) ]
  , [ prefix "fix" Fix ]
  , [ prefix "id" (App $ Lam "x" (Var "x")) ]
  , [prefix "const" (App $ Lam "x" (Lam "y" (Var "x")))]]

term :: Parser RExpr
term = makeExprParser pTerm operatorTable 

-- If multiple terms are given, they are applied to each other from left to right.
pExprs :: Parser RExpr
-- don't allow "let", or "in" as a variable name
-- an expression also ends when a ; is encountered
pExprs = foldl1 App <$> some term <* optional (symbol ";")

-- parser for lambda expr
pLambda :: Parser RExpr
pLambda = do
  symbol "\\" <|> symbol "λ"
  args <- some identifier
  symbol "->"
  body <- pExprs
  pure $ foldr Lam body args

-- let expr parser
-- let name = expr in expr'
pLetExpr :: Parser RExpr
pLetExpr = do
  _ <- symbol "let"
  name <- identifier
  _ <- symbol "="
  expr <- pExprs 
  _ <- symbol "in"
  expr' <- pExprs
  pure $ Let name expr expr'

type Binding = (Name, RExpr)

-- parser for boolean literals
pBool :: Parser RExpr
pBool = choice
  [ symbol "true"  *> pure (Lit (LBool True))
  , symbol "false" *> pure (Lit (LBool False)) ]

-- parser for if then else expr
ifThenElse :: Parser RExpr
ifThenElse = If <$> (symbol "if" *> term)
               <*> (symbol "then" *> term)
               <*> (symbol "else" *> term)

-- parser for top level declarations
-- form is `let name args... = body`
pLetDecl :: Parser Binding
pLetDecl = do
  symbol "let"
  name <- identifier
  traceM $ "name: " <> show name
  args <- many identifier
  traceM $ "args: " <> show args
  symbol "="
  body <- pExprs
  traceM $ "body: " <> show body
  pure (name, foldr Lam body args)

raupekaModule :: Parser [Binding]
-- A module is a list of top level declarations
raupekaModule = many pLetDecl

parseExprs :: FilePath -> Text -> Either (ParseErrorBundle Text Void) RExpr
parseExprs file input = runParser pExprs file input

parseModule :: FilePath -> Text -> Either (ParseErrorBundle Text Void) [Binding]
parseModule file input = runParser raupekaModule file input
