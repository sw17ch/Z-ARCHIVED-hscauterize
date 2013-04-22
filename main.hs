{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as TO

import Text.Parsec
import Data.Cauterize.Parser

main :: IO ()
main = do
  d <- TO.readFile fname
  case parse parseCauterize fname d of
    Left e -> print e
    Right v -> print v
  where
    fname = "example.scm"
