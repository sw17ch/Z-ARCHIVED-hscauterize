{-# LANGUAGE OverloadedStrings #-}
module Data.Cauterize.Generators.C ( gen ) where

import Data.Cauterize.Specification
import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))
import qualified Data.Text as T

gen :: Spec -> P.Doc
gen (Spec n v h rs) =
  P.text (T.unpack n) <+> P.text (T.unpack v) <+> P.text (T.unpack h)
