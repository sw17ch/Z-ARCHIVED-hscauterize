{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cauterize.Generators.C where

import Data.Text (unpack)
import Data.Cauterize.Types
import qualified Data.Map as M
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

    rules :: [C.Definition]
    rules = map genRule (M.elems rs)

genRule :: CauterizeRule -> C.Definition
genRule (CauterizeType t) = genType t

genType :: CautType -> C.Definition
genType (CautScalar s) = genScalar s
genType (CautEnumeration e) = genEnumeration e
genType (CautFixed a) = genFixedArray a
genType (CautBounded a) = genBoundedArray a
genType (CautComposite c) = genComposite c
genType _ = [cedecl|struct some_unknown_type;|]

genScalar :: Scalar -> C.Definition
genScalar (Scalar name typ) = [cedecl|typedef $ty:typ' $id:(unpack name);|]
  where
    typ' = [cty|typename $id:(unpack typ)|]
    -- There's a comment in Language/C/Quote.hs that describes using 'typename'
    -- to introduce an identifier that represents a type that may not yet be in
    -- scope. That's what we're doing here to construct our typedef.

genEnumeration :: Enumeration -> C.Definition
genEnumeration (Enumeration name values) =
  [cedecl|
    enum $id:(unpack name) {
      $enums:(map asC values)
    };
  |]
  where
    asC (EnumValue valn Nothing) = [cenum|$id:(unpack valn) |]
    asC (EnumValue valn (Just valc)) =
      case valc of
        (HexConst i _) -> [cenum|$id:(unpack valn) = $hexint:(i) |]
        (DecConst i _) -> [cenum|$id:(unpack valn) = $int:(i) |]
        (OctConst i _) -> [cenum|$id:(unpack valn) = $int:(i) |]
        (BinConst i _) -> [cenum|$id:(unpack valn) = $int:(i) |]

anInt :: C.Type
anInt = [cty|int|]

genFixedArray :: FixedArray -> C.Definition
genFixedArray (FixedArray name member_name len) =
  [cedecl|
     struct $id:(unpack name) {
       $ty:arTy data;
     };
   |]
  where
    arTy = let t = [cty|typename $id:(unpack member_name)|]
               s = [cexp|$(constVal len)|]
           in [cty|$ty:t dummy[$exp:s]|]

arrLenToRep :: Integer -> C.Type
arrLenToRep len | between (0   :: Integer) (u08 - 1 :: Integer) = tn ("uint8_t"  :: String)
                | between (u08 :: Integer) (u16 - 1 :: Integer) = tn ("uint16_t" :: String)
                | between (u16 :: Integer) (u32 - 1 :: Integer) = tn ("uint32_t" :: String) 
                | between (u32 :: Integer) (u64 - 1 :: Integer) = tn ("uint64_t" :: String) 
                | otherwise = error "Array length out of bounds."
  where
    between :: Integer -> Integer -> Bool
    between low high = low <= len && len <= high
    tn s = [cty|typename $id:s|] 

    u08, u16, u32, u64 :: Integer
    u08 = (2 :: Integer) ^ (8  :: Integer)
    u16 = (2 :: Integer) ^ (16 :: Integer)
    u32 = (2 :: Integer) ^ (32 :: Integer)
    u64 = (2 :: Integer) ^ (64 :: Integer)

genBoundedArray :: BoundedArray -> C.Definition
genBoundedArray (BoundedArray name member_name max_len) =
  let clen = constVal max_len
  in [cedecl|
        struct $id:(unpack name) {
          $ty:(arrLenToRep clen) length;
          $ty:arTy data;
        };
      |]
  where
    arTy = let t = [cty|typename $id:(unpack member_name)|]
               s = [cexp|$(constVal max_len)|]
           in [cty|$ty:t dummy[$exp:s]|]

genComposite :: Composite -> C.Definition
genComposite (Composite name fields) = 
  [cedecl|
    struct $id:(unpack name) {
      int oh_man_there_is_a_lot_to_do_here;
    };
  |]

  where
    fieldDef (Field fname tname) =
      let t = [cty|typename $id:(unpack tname)|]
      in [cedecl|$ty:t $id:(unpack fname);|]

-- grp = let (di, dt) = dat
--           (ti, tt) = tag
--           td = [csdecl|$ty:tt $id:ti;|]
--           ud = [csdecl|$ty:dt $id:di;|]
--       in [cdecl|struct foo_grp { $sdecl:td $sdecl:ud };|]
