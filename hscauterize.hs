{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as TO

import Text.Parsec
import Data.Cauterize.Parsers.Schema
import Data.Cauterize.Types.Specification
import Options.Applicative

import qualified Text.PrettyPrint.HughesPJClass as P

data CautOpts = CautOpts
  { inputFile :: String
  } deriving (Show)


optParser :: Parser CautOpts
optParser = CautOpts
  <$> strOption
    ( long "input"
   <> metavar "FILE_PATH"
   <> help "Input Cauterize schema file."
    )

opts :: ParserInfo CautOpts
opts = info (optParser <**> helper)
  ( fullDesc
 <> progDesc "Process Cauterize schema files."
  )

main :: IO ()
main = do
  o <- execParser opts
  d <- TO.readFile $ inputFile o

  case parse parseSchema (inputFile o) d of
    Left e -> print e
    Right s -> do
      print $ P.pPrint s
      putStrLn "=="
      print $ P.pPrint $ fromSchema s
