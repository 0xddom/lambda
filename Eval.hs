module Eval (
  evalTree
) where

import Symbols
import Ast
import Lambda
import Text.Parsec

data Action = Action LambdaTerm Reduction

instance Show Action where
  show (Action l r) = show l ++ " " ++ show r ++ "\n"

tupleToAction :: (LambdaTerm, Reduction) -> Action
tupleToAction (l,r) = Action l r

lambdaTermFromAction :: Action -> LambdaTerm
lambdaTermFromAction (Action l _) = l

reductionFromAction :: Action -> Reduction
reductionFromAction (Action _ r) = r

evalLambdaTerm' :: LambdaTerm -> [Action]
evalLambdaTerm' m = let result = (tupleToAction . reduceLambdaTerm $ m) in
      (Action m (reductionFromAction result)):
      -- Break if the result is equal after the reduction and is not an Alpha reduction
        if m == (lambdaTermFromAction result) && reductionFromAction result /= Alpha
        then [Action m None]
        else (evalLambdaTerm $ lambdaTermFromAction result)

evalLambdaTerm :: LambdaTerm -> [Action]
evalLambdaTerm m
  | canEtaReduce m = evalLambdaTerm' m
  | normalFormP m = [Action m None]
  | otherwise = evalLambdaTerm' m

joinActions :: [Action] -> IO String
joinActions a = 
  return $ (foldr (++) "") . (map show) $ a

evalTree :: Either ParseError ParseTree -> IO String
evalTree (Left parseError) = 
  return $ show parseError
  
evalTree (Right tree) = 
  convertTree tree >>=
  (joinActions . evalLambdaTerm)
  
convertTree :: ParseTree -> IO LambdaTerm
convertTree tree = 
  return $ cstToAst tree
