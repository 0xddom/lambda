module Lambda.Ast (
  LambdaTerm(Application,Abstraction,FreeVariable),
  Variable,
  Reduction(Alpha,Beta,Eta,None),
  appl,
  abst,
  free,
  isAbst,
  body
) where

type Variable = String

data LambdaTerm =
    Application LambdaTerm LambdaTerm
  | Abstraction Variable LambdaTerm
  | FreeVariable Variable

data Reduction = Alpha | Beta | Eta | None

instance Eq Reduction where
  Alpha == Alpha = True
  Beta == Beta = True
  Eta == Eta = True
  None == None = True
  _ == _ = False

instance Show Reduction where
  show Alpha = "-- α -->"
  show Beta = "-- β -->"
  show Eta = "-- η -->"
  show None = ""

instance Show LambdaTerm where
  show (FreeVariable x) = x
  show (Abstraction x m) = "λ" ++ x ++ "." ++ show m
  show (Application (FreeVariable x) n) = x ++ " " ++ show n
  show (Application m n) = "(" ++ show m ++ ")" ++ show n

instance Eq LambdaTerm where
  (FreeVariable _) == (FreeVariable _) = True
  (Abstraction _ m) == (Abstraction _ n) = m == n
  (Application m n) == (Application m' n') = m == m' && n == n'
  _ == _ = False

appl :: LambdaTerm -> LambdaTerm -> LambdaTerm
appl = Application

abst :: Variable -> LambdaTerm -> LambdaTerm
abst = Abstraction

free :: Variable -> LambdaTerm
free = FreeVariable

isAbst :: LambdaTerm -> Bool
isAbst (Abstraction _ _) = True
isAbst _ = False

body :: LambdaTerm -> LambdaTerm
body (Abstraction _ m) = m
body (Application m _) = m
body m = m
