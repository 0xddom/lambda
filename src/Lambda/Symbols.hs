module Lambda.Symbols(
  Lambda(Lambda),
  Dot(Dot),
  LParen(LParen),
  RParen(RParen),
  FreeVar(FreeVar),
  ParseTree(AbstrExpr,FreeVarExpr,ApplExpr,NumberExpr),
  abstrExpr,
  cstToAst
) where

import Lambda.Ast

data Lambda = Lambda
data FreeVar = FreeVar Variable
data Dot = Dot
data LParen = LParen
data RParen = RParen

instance Show Lambda where show Lambda = "lambda"
instance Show Dot where show Dot = "."
instance Show LParen where show LParen = "("
instance Show RParen where show RParen = ")"

data ParseTree =
    AbstrExpr [Variable] ParseTree
  | FreeVarExpr FreeVar
  | ApplExpr ParseTree ParseTree
  | NumberExpr Int

instance Show ParseTree where
  show (AbstrExpr vars _body) =
    "(λ (" ++ concat vars ++ ") " ++ show _body ++ ")"
  show (FreeVarExpr (FreeVar x)) = x
  show (ApplExpr m n) = "(" ++ show m ++ " " ++ show n ++ ")"
  show (NumberExpr n) = show n

abstrExpr :: [Variable] -> ParseTree -> ParseTree
abstrExpr = AbstrExpr

buildChurchNum :: LambdaTerm -> LambdaTerm -> LambdaTerm
buildChurchNum _ = appl (free "f")

infiniteFreeVars :: String -> [LambdaTerm]
infiniteFreeVars name = free name:infiniteFreeVars name

buildAbstraction :: LambdaTerm -> LambdaTerm -> LambdaTerm
buildAbstraction (FreeVariable x) acc = abst x acc
buildAbstraction m _ = m

cstToAst :: ParseTree -> LambdaTerm
cstToAst (FreeVarExpr (FreeVar x)) = free x
cstToAst (AbstrExpr x m) = foldr (buildAbstraction . free) (cstToAst m) x
cstToAst (ApplExpr m n) = appl (cstToAst m) (cstToAst n)
cstToAst (NumberExpr n) =
  abst "f" $ abst "x"
  $ foldr buildChurchNum (free "x") (take n $ infiniteFreeVars "f")
