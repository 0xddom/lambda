module Lambda.Eval (
  evalTree
) where

import Lambda.Symbols
import Lambda.Ast
import Lambda.Reducer
import Text.Parsec

data Action = Action Int LambdaTerm Reduction

instance Show Action where
  show (Action n l r) = show n ++ ": " ++ show l ++ " " ++ show r ++ "\n"

tupleToAction :: (LambdaTerm, Reduction) -> Action
tupleToAction (l,r) = Action 0 l r

lambdaTermFromAction :: Action -> LambdaTerm
lambdaTermFromAction (Action _ l _) = l

reductionFromAction :: Action -> Reduction
reductionFromAction (Action _ _ r) = r

evalLambdaTerm' :: Int -> LambdaTerm -> [Action]
evalLambdaTerm' step m = let result = (tupleToAction . reduceLambdaTerm $ m) in
      Action step m (reductionFromAction result):
      -- Break if the result is equal after the reduction and is not an Alpha reduction
        if m == lambdaTermFromAction result && reductionFromAction result /= Alpha
        then [Action (step + 1) m None]
        else evalLambdaTerm (step + 1) $ lambdaTermFromAction result

evalLambdaTerm :: Int -> LambdaTerm -> [Action]
evalLambdaTerm step m
  | canEtaReduce m = evalLambdaTerm' step m
  | normalFormP m = [Action step m None]
  | otherwise = evalLambdaTerm' step m

joinActions :: [Action] -> IO String
joinActions a =
  return $ foldr ((++) . show) "" a
 -- return $ (foldr (++) "") . (map show) $ a

evalTree :: Either ParseError ParseTree -> IO String
evalTree (Left parseError) = 
  return $ show parseError
  
evalTree (Right tree) = 
  convertTree tree >>=
  (joinActions . evalLambdaTerm 1)
  
convertTree :: ParseTree -> IO LambdaTerm
convertTree tree = 
  return $ cstToAst tree
