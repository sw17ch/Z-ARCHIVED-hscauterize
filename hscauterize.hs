{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as TO

import Text.Parsec
import Data.Cauterize.Parser
import Data.Cauterize.Generators.C

main :: IO ()
main = do
  d <- TO.readFile fname
  case parse parseCauterize fname d of
    Left e -> print e
    Right v -> print $ gen v
  where
    fname = "example.scm"
