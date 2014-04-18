module Data.Cauterize.Parsers.Utils ( parens , quoted  , spaces' ) where

import Text.Parsec
import Text.Parsec.Text

import Data.Text

import Control.Monad (liftM)

parens :: Parser a -> Parser a
parens a = do
  _ <- char '('
  a' <- a
  _ <- char ')'
  return a'

quoted :: Parser Text
quoted = do
  _ <- char '"'
  liftM pack $ manyTill anyToken (char '"')

spaces' :: Parser ()
spaces' = space >> spaces
