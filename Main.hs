module Main(main) where

import Eval
import Parser

banner :: IO ()
banner = 
  putStrLn "Lambda Calculus reducer"

main :: IO ()
main =
  banner >>
  getContents >>=
  parseExpr "stdin" >>=
  evalTree >>=
  putStrLn
  
