module Parser (
  parseExpr
) where

import Text.Parsec
import Text.Parsec.String
import Symbols
import Ast

lambda :: Parser Lambda
lambda = do
  _ <- char 'λ' <|> char '\\'
  return $ Lambda

dot :: Parser Dot
dot = do
  _ <- char '.'
  return $ Dot

var :: Parser Variable
var = do
  name <- lower
  trail <- many $ char '\''
  return $ name:trail

lparen :: Parser LParen
lparen = do
  _ <- char '('
  return $ LParen

rparen :: Parser RParen
rparen = do
  _ <- char ')'
  return $ RParen

abstractionExpr :: Parser ParseTree
abstractionExpr = do
  _ <- lambda
  _var <- many1 var
  _ <- dot
  _body <- lambdaProg
  return $ abstrExpr _var _body

freeVarExpr :: Parser ParseTree
freeVarExpr = do
  _var <- var
  return $ FreeVarExpr $ FreeVar _var

applicationExpr :: Parser ParseTree
applicationExpr = do
  lval <- wrappedExpr
  rval <- nonApplicativeExpr
  return $ ApplExpr lval rval

numberExpr :: Parser ParseTree 
numberExpr = do
  num <- many1 digit
  return $ NumberExpr $ read num

wrappedExpr :: Parser ParseTree
wrappedExpr = do
  _ <- lparen
  expr <- lambdaProg
  _ <- rparen
  return expr

nonApplicativeExpr :: Parser ParseTree
nonApplicativeExpr =
   try abstractionExpr
   <|> (try freeVarExpr)
   <|> (try numberExpr)
   <|> wrappedExpr

lambdaProg :: Parser ParseTree
lambdaProg =
  try applicationExpr
  <|> nonApplicativeExpr

parseExpr :: String -> String -> IO (Either ParseError ParseTree)
parseExpr source expr = do
  return $ parse lambdaProg source expr
