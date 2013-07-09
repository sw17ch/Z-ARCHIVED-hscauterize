{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cauterize.Generators.C where

import Data.Text (unpack)
import Data.Cauterize.Types
import Language.C.Quote.C
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland

gen :: Cauterize -> Doc
gen (Cauterize n v rs) = ppr ci </> (stack $ map ppr rules)
  where
    name = unpack $ unName n
    version = unpack $ unVersion v

    ci = [cedecl|
            const struct cauterize_info info = {
              .name = $string:name,
              .version = $string:version,
            };
         |]

    rules = map genRule rs

genRule :: CauterizeRule -> C.Definition
genRule (CauterizeType t) = genType t

genType :: CautType -> C.Definition
genType (CautScalar s) = genScalar s
genType _ = [cedecl|struct some_unknown_type;|]

genScalar :: Scalar -> C.Definition
genScalar (Scalar name typ) = [cedecl|typedef $ty:typ' $id:(unpack name);|]
  where
    typ' = [cty|typename $id:(unpack typ)|]
    -- There's a comment in Language/C/Quote.hs that describes using 'typename'
    -- to introduce an identifier that represents a type that may not yet be in
    -- scope. That's what we're doing here to construct our typedef.

anInt :: C.Type
anInt = [cty|int|]

-- grp = let (di, dt) = dat
--           (ti, tt) = tag
--           td = [csdecl|$ty:tt $id:ti;|]
--           ud = [csdecl|$ty:dt $id:di;|]
--       in [cdecl|struct foo_grp { $sdecl:td $sdecl:ud };|]
