module Args(
  Arguments(Arguments,filePath,expression),
  arguments) where

import Options.Applicative
import Data.Monoid ((<>))

data Arguments  = Arguments {
  filePath :: Maybe String,
  expression :: Maybe String
} deriving (Show)

argsParse :: Parser Arguments
argsParse = Arguments
  <$> ( optional $ strOption
       ( metavar "EXPRESSION"
       <> long "expr"
       <> short 'e'
       <> help "A λ expression" ))
  <*> ( optional $ strOption
       ( metavar "FILE"
       <> long "file"
       <> short 'f'
       <> help "A file to read from. Use - to read from stdin" ))

arguments :: ParserInfo Arguments
arguments = info (helper <*> argsParse)
  (fullDesc
  <> progDesc "A λ-calculus interpreter and reducer designed for teaching and learning of λ-calculus."
  <> header "lambda - A simple λ-calculus interpreter and reducer" )
