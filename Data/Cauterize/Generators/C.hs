{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Cauterize.Generators.C ( gen ) where

import Data.Text (unpack)
import Data.Cauterize.Types
import qualified Data.Map as M
import Language.C.Quote.C
import qualified Language.C.Syntax as C
import Text.PrettyPrint.Mainland
import Control.Monad.Reader

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
    rules = map (\r -> runReader (genRule r) rs) (M.elems rs)

genRule :: CauterizeRule -> Reader (M.Map TypeName CauterizeRule) C.Definition
genRule (CauterizeType t) = genType t

genType :: CautType -> Reader (M.Map TypeName CauterizeRule) C.Definition
genType (CautScalar s) = genScalar s
genType (CautEnumeration e) = genEnumeration e
genType (CautFixed a) = genFixedArray a
genType (CautBounded a) = genBoundedArray a
genType (CautComposite c) = genComposite c
genType (CautGroup g) = genGroup g

genScalar :: Scalar -> Reader (M.Map TypeName CauterizeRule) C.Definition
genScalar (Scalar name typ) = return [cedecl|typedef $ty:typ' $id:(unpack name);|]
  where
    typ' = [cty|typename $id:(unpack typ)|]
    -- There's a comment in Language/C/Quote.hs that describes using 'typename'
    -- to introduce an identifier that represents a type that may not yet be in
    -- scope. That's what we're doing here to construct our typedef.

genEnumeration :: Enumeration -> Reader (M.Map TypeName CauterizeRule) C.Definition
genEnumeration (Enumeration name values) =
  return [cedecl|
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

typeNameToType :: (M.Map TypeName CauterizeRule) -> TypeName -> C.Type
typeNameToType c n = ruleToType r
  where
    r = case M.lookup n c of
          (Just r') -> r'
          Nothing -> error $ "ERROR: No type named " ++ (unpack n)

ruleToType :: CauterizeRule -> C.Type
ruleToType (CauterizeType (CautScalar (Scalar n _))) = [cty|typename $id:(unpack n)|]
ruleToType (CauterizeType (CautEnumeration (Enumeration n _))) = [cty|enum $id:(unpack n)|]
ruleToType (CauterizeType (CautFixed (FixedArray n _ _))) = [cty|struct $id:(unpack n)|]
ruleToType (CauterizeType (CautBounded (BoundedArray n _ _))) = [cty|struct $id:(unpack n)|]
ruleToType (CauterizeType (CautComposite (Composite n _))) = [cty|struct $id:(unpack n)|]
ruleToType (CauterizeType (CautGroup (Group n _))) = [cty|struct $id:(unpack n)|]

genFixedArray :: FixedArray -> Reader (M.Map TypeName CauterizeRule) C.Definition
genFixedArray (FixedArray name member_name len) = do
  c <- ask
  return [cedecl|
     struct $id:(unpack name) {
       $ty:(arTy c) data;
     };
   |]
  where
    arTy c = let t = typeNameToType c member_name
                 s = [cexp|$(constVal len)|]
             in [cty|$ty:t dummy[$exp:s]|]

arrLenToRep :: Integer -> C.Type
arrLenToRep len | between 0   (u08 - 1) = tn ("uint8_t"  :: String)
                | between u08 (u16 - 1) = tn ("uint16_t" :: String)
                | between u16 (u32 - 1) = tn ("uint32_t" :: String) 
                | between u32 (u64 - 1) = tn ("uint64_t" :: String) 
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

genBoundedArray :: BoundedArray -> Reader (M.Map TypeName CauterizeRule) C.Definition
genBoundedArray (BoundedArray name member_name max_len) = do
  c <- ask
  return [cedecl|
     struct $id:(unpack name) {
       $ty:(arrLenToRep clen) length;
       $ty:(arTy c) data;
     };
   |]
  where
    clen = constVal max_len
    arTy c = let t = typeNameToType c member_name
                 s = [cexp|$(constVal max_len)|]
             in [cty|$ty:t dummy[$exp:s]|]

fieldDef :: M.Map TypeName CauterizeRule -> Field -> C.FieldGroup
fieldDef c (Field fname tname) =
  let t = typeNameToType c tname
  in [csdecl|$ty:t $id:(unpack fname);|]

genComposite :: Composite -> Reader (M.Map TypeName CauterizeRule) C.Definition
genComposite (Composite name fields) = do
  c <- ask
  return [cedecl|
    struct $id:(unpack name) { $sdecls:(map (fieldDef c) fields) };
  |]

fieldToTagName :: String -> Field -> String
fieldToTagName prefix (Field n _) = prefix ++ (unpack n)

fieldsToTagPairs :: String -> [Field] -> [(String, Integer)]
fieldsToTagPairs prefix fs = let names = map (fieldToTagName prefix) fs
                             in zip names [0..]

genGroup :: Group -> Reader (M.Map TypeName CauterizeRule) C.Definition
genGroup (Group name fields) = do
  c <- ask
  return [cedecl|
    struct $id:(unpack name) {
      enum { $enums:tags } tag;
      union { $sdecls:(map (fieldDef c) fields) } data;
    };
  |]
  where
    tag_prefix = "GROUP_" ++ (unpack name) ++ "_TAG_"
    tags = map (\(n, i) -> [cenum|$id:n = $int:i|]) (fieldsToTagPairs tag_prefix fields)
