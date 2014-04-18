module Data.Cauterize.Parsers.Utils ( parens , quoted  , spaces' ) where

import Text.Parsec
import Text.Parsec.Text

parens :: Parser a -> Parser a
parens a = do
  _ <- char '('
  a' <- a
  _ <- char ')'
  return a'

quoted :: Parser String
quoted = do
  _ <- char '"'
  manyTill anyToken (char '"')

spaces' :: Parser ()
spaces' = space >> spaces
