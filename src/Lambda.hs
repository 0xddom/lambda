module Main(main) where

import Lambda.Eval
import Lambda.Parser
import System.IO
import Lambda.Args as A
import Options.Applicative

type Content = Either (String -> IO Handle) String

getHandle :: Arguments -> IO Content
getHandle (Arguments _ (Just _filePath))
  | _filePath == "-" = return $ Left $ \_ -> return stdin
  | otherwise = return $ Left $ \_ -> openFile _filePath ReadMode
getHandle (Arguments (Just expr) _) = return $ Right expr
getHandle (Arguments Nothing Nothing) = return $ Right ""

getFileName :: Arguments -> String
getFileName (Arguments (Just _) _) = "expr"
getFileName (Arguments Nothing (Just _filePath))
  | _filePath == "-" = "stdin"
  | otherwise = _filePath
getFileName _ = "null"

getContent :: Content -> IO String
getContent (Left handle) = handle "" >>= hGetContents
getContent (Right expr) = return expr

mainWithArgs :: Arguments -> IO ()
mainWithArgs (Arguments Nothing Nothing) =
  mainWithArgs (Arguments Nothing (Just "-"))
mainWithArgs args = 
  getHandle args >>=
  getContent  >>=
  parseExpr (getFileName args)  >>=
  evalTree  >>=
  putStrLn 
  
main :: IO () 
main = execParser arguments >>= mainWithArgs
   
