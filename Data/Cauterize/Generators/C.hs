{-# LANGUAGE OverloadedStrings #-}
module Data.Cauterize.Generators.C ( gen ) where

import Data.Cauterize.Types.Specification
import qualified Text.PrettyPrint as P
import Text.PrettyPrint ((<+>))

gen :: Spec -> P.Doc
gen (Spec n v h _) =
  P.text n <+> P.text v <+> P.text h
