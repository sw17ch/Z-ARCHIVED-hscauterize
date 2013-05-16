{-# LANGUAGE QuasiQuotes #-}
module Data.Cauterize.Generators.C where

import Text.PrettyPrint.Mainland
import Language.C.Quote.C

genC :: IO ()
genC = print $ ppr f
  where
    ty = [cty|int|]
    idnt = "x"
    fname = "foo"
    param = [cparam|$ty:ty $id:idnt|]

    f = [cfun|$ty:ty $id:fname($param:param) { return $id:idnt; }|]

