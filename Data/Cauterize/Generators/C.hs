{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cauterize.Generators.C ( gen ) where

import Data.Cauterize.Types
import qualified Text.PrettyPrint as P
import qualified Data.Text as T

gen :: Cauterize -> P.Doc
gen c = (P.text . T.unpack . unName) (cauterizeSchemaName c)
