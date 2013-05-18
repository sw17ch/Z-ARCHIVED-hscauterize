{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cauterize.Generators.C where

import Data.Text (unpack)
import Data.Cauterize.Types
import Language.C.Quote.C
import Text.PrettyPrint.Mainland

genC :: Cauterize -> Doc
genC (Cauterize n v _) = ppr ci
  where
    name = unpack $ unName n
    version = unpack $ unVersion v

    ci = [cedecl|
            struct cauterize_info info = {
              .name = $id:name,
              .version = $id:version,
            };
         |]

-- tag = let i = "tag"
--           t = [cty|enum foo_grp_tag { a, b, }|]
--       in (i, t)
-- 
-- dat = let i = "data"
--           t = [cty|union { int a; int b; }|]
--       in (i, t)
-- 
-- grp = let (di, dt) = dat
--           (ti, tt) = tag
--           td = [csdecl|$ty:tt $id:ti;|]
--           ud = [csdecl|$ty:dt $id:di;|]
--       in [cty|struct foo_grp { $sdecl:td $sdecl:ud }|]
