module Lambda(
  normalFormP,
  reduceLambdaTerm,
  canEtaReduce
) where

import Data.List as L
import Ast
import Utils

bindVars :: LambdaTerm -> [Variable]
bindVars (FreeVariable _) = []
bindVars (Abstraction x m) = x:(bindVars m)
bindVars (Application m n) = (bindVars m) ++ (bindVars n)

freeVars :: LambdaTerm -> [Variable]
freeVars (FreeVariable x) = [x]
freeVars (Abstraction x m) = filter (/= x) (freeVars m)
freeVars (Application m n) = removeDups $ (freeVars m) ++ (freeVars n)

consistent :: LambdaTerm -> LambdaTerm -> Bool
consistent m n = (== []) $ bindVars m `L.intersect` freeVars n

replace :: LambdaTerm -> LambdaTerm -> Variable -> LambdaTerm
replace (FreeVariable x) l y
  | x == y = l
  | otherwise = FreeVariable x
replace (Abstraction x m) l y
  | x == y = Abstraction x m
  | otherwise = Abstraction x $ replace m l y
replace (Application m n) l y = Application (replace m l y) (replace n l y)

modifyBindVariables :: [Variable] -> LambdaTerm -> LambdaTerm
modifyBindVariables fv (Application m n) =
  appl (modifyBindVariables fv m) (modifyBindVariables fv n)
modifyBindVariables fv (FreeVariable x)
  | x `elem` fv = free $ newNameForVar x fv
  | otherwise = free x
modifyBindVariables fv (Abstraction x m)
  | x `elem` fv = abst (newNameForVar x fv) (modifyBindVariables fv m)
  | otherwise = abst x (modifyBindVariables fv m)

alphaReduction :: LambdaTerm -> LambdaTerm
alphaReduction (Application m n) = appl (modifyBindVariables (freeVars n) m) n
alphaReduction m = m

betaReduction :: LambdaTerm -> LambdaTerm
betaReduction (Application (Abstraction x m) n) = replace m n x
betaReduction (Application (Application m n') n) =
  appl (betaReduction $ appl m n') n
betaReduction m = m

etaReduction :: LambdaTerm -> LambdaTerm
etaReduction (Abstraction x (Application m (FreeVariable y)))
  | x == y && not (x `elem` (freeVars m)) = m
  | otherwise = (Abstraction x (Application m (FreeVariable y)))
etaReduction m = m

normalFormP :: LambdaTerm -> Bool
normalFormP (FreeVariable _) = True
normalFormP (Abstraction _ _) = True
normalFormP (Application (FreeVariable _) _) = True
normalFormP (Application _ _) = False

betaReductionWillCollide :: LambdaTerm -> Bool
betaReductionWillCollide (Application m n)
  | isAbst m = not $ consistent (body m) n
  | otherwise = betaReductionWillCollide (body m)
betaReductionWillCollide _ = False

canBetaReduce :: LambdaTerm -> Bool
canBetaReduce (Application m _)
  | isAbst m = True
  | otherwise = canBetaReduce m
canBetaReduce _ = False

canEtaReduce :: LambdaTerm -> Bool
canEtaReduce (Abstraction x (Application m (FreeVariable y))) =
  x == y && not (x `elem` (freeVars m))
canEtaReduce _ = False

reduceLambdaTerm :: LambdaTerm -> (LambdaTerm, Reduction)
reduceLambdaTerm m
  | betaReductionWillCollide m = (alphaReduction m, Alpha)
  | canBetaReduce m = (betaReduction m, Beta)
  | canEtaReduce m = (etaReduction m, Eta)
  | otherwise = (m, None)

-- XXX: Write a lot of unit tests
