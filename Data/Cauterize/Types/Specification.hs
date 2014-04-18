{-# LANGUAGE DeriveDataTypeable #-}

module Data.Cauterize.Types.Specification
  ( Spec(..)
  , SpecName
  , SpecVersion
  , SpecHash
  , fromSchema
  ) where

import Data.Cauterize.Types.Schema
import Data.Cauterize.Types.BuiltIn
import Data.Cauterize.Types.TypeName
import Data.Cauterize.Types.Field
import Data.Data
import Data.List
import qualified Data.Map as M
import qualified Crypto.Hash as H

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

type SpecName = String
type SpecVersion = String
type SpecHash = String

fromSchema :: Schema -> Spec
fromSchema (Schema n v ts) =
  let s = Spec n v (mkSpecHash s) (fromTypes ts) in s

mkSpecHash :: Spec -> String
mkSpecHash _ = "0000000000000000000000000000000000000000"

maxu8, maxu16, maxu32, maxu64 :: Integer
maxu8  = (2 :: Integer)^(8 :: Integer) - 1
maxu16 = (2 :: Integer)^(16 :: Integer) - 1
maxu32 = (2 :: Integer)^(32 :: Integer) - 1
maxu64 = (2 :: Integer)^(64 :: Integer) - 1

maxs8, maxs16, maxs32, maxs64 :: Integer
maxs8  = (2 :: Integer)^(7 :: Integer) - 1
maxs16 = (2 :: Integer)^(15 :: Integer) - 1
maxs32 = (2 :: Integer)^(31 :: Integer) - 1
maxs64 = (2 :: Integer)^(63 :: Integer) - 1

mins8, mins16, mins32, mins64 :: Integer
mins8  = -(2 :: Integer)^(7 :: Integer)
mins16 = -(2 :: Integer)^(7 :: Integer)
mins32 = -(2 :: Integer)^(7 :: Integer)
mins64 = -(2 :: Integer)^(7 :: Integer)

reprFromCount :: Integer -> BuiltIn
reprFromCount v | v <= maxu8  = BiUint8
                | v <= maxu16 = BiUint16
                | v <= maxu32 = BiUint32
                | v <= maxu64 = BiUint64
                | otherwise = error $ "Cannot represent the value " ++ show v ++ "."

reprFromBounds :: Integer -> Integer -> BuiltIn
reprFromBounds mi ma | 0 <= mi && ma <= maxu8 = BiUint8
                     | 0 <= mi && ma <= maxu16 = BiUint16
                     | 0 <= mi && ma <= maxu32 = BiUint32
                     | 0 <= mi && ma <= maxu64 = BiUint64
                     | mins8  <= mi && ma <= maxs8  = BiInt8
                     | mins16 <= mi && ma <= maxs16 = BiInt16
                     | mins32 <= mi && ma <= maxs32 = BiInt32
                     | mins64 <= mi && ma <= maxs64 = BiInt64
                     | otherwise = error $ "Cannot represent the values " ++ show mi ++ " and " ++ show ma ++ "."

fromTypes :: M.Map TypeName SchemaType -> M.Map TypeName SpecType
fromTypes = M.map fromType

fromType :: SchemaType -> SpecType
-- Scalars have no meta-data
fromType (SchemaScalar name target) = SpecScalar name target
-- Enumerations need to assign constants to each member at this stage
-- TODO: This is incomplete
fromType (SchemaEnumeration name values) =
  SpecEnumeration name enums (reprFromBounds (minimum reprs) (maximum reprs))
  where
    enums = fromEnumValues values
    reprs = map (\(EnumSpecValue _  c) -> constVal c) enums
-- Fixed arrays have no meta data
fromType (SchemaFixedArray name target cnst) = SpecFixed name target cnst
-- Bounded arrays need to determine the bits needed to store the length
fromType (SchemaBoundedArray name target cnst) =
  SpecBounded name target cnst (reprFromCount $ constVal cnst)
-- Composites have no meta-data
fromType (SchemaComposite name fields) = SpecComposite name fields
-- Groups need to determine how many bits are needed to represent the type tag
fromType (SchemaGroup name fields) =
  SpecGroup name fields (reprFromCount $ fromIntegral $ length fields)

fromEnumValues :: [EnumValue] -> [EnumSpecValue]
fromEnumValues vs = let c = Const 0
                    in snd $ mapAccumL go c vs
  where
    go :: Const -> EnumValue -> (Const, EnumSpecValue)
    go _ (EnumValue n (Just c)) = (Const (constVal c + 1), EnumSpecValue n c)
    go c (EnumValue n Nothing) = let cv = constVal c
                                     cv' = cv + 1
                                 in (Const cv', EnumSpecValue n (Const cv))

data Spec = Spec
  { specName :: SpecName
  , specVersion :: SpecVersion
  , specHash :: SpecHash
  , specTypes :: M.Map TypeName SpecType
  }
  deriving (Show, Data, Typeable)

data SpecType = SpecScalar TypeName TypeName
              | SpecEnumeration TypeName [EnumSpecValue] BuiltIn
              | SpecFixed TypeName TypeName Const
              | SpecBounded TypeName TypeName Const BuiltIn
              | SpecComposite TypeName [Field]
              | SpecGroup TypeName [Field] BuiltIn
  deriving (Show, Data, Typeable)

data EnumSpecValue = EnumSpecValue TypeName Const
  deriving (Show, Data, Typeable)


instance Pretty Spec where
  pPrint (Spec n v h ts) = parens $ hang pSpec 1 pTypes
    where
      pSpec = text "specification" <+> text n <+> text v <+> text h 
      pTypes = vcat $ map pPrint (M.elems ts)

instance Pretty SpecType where
  pPrint (SpecScalar name target) = parens $ text "scalar" <+> text name <+> text target
  pPrint (SpecEnumeration name values repr) = parens $ hang pEnum 1 pValues
    where
      pEnum = text "enumeration" <+> text name <+> pPrint repr
      pValues = vcat $ map pPrint values
  pPrint (SpecFixed name target len) = parens $ text "fixed" <+> text name <+> text target <+> pPrint len
  pPrint (SpecBounded name target maxLen repr) = parens $ text "bounded" <+> text name <+> text target <+> pPrint maxLen <+> pPrint repr
  pPrint (SpecComposite name fields) = parens $ hang pComp 1 pFields
    where
      pComp = text "composite" <+> text name
      pFields = vcat $ map pPrint fields
  pPrint (SpecGroup name fields repr) = parens $ hang pGroup 1 pFields
    where
      pGroup = text "group" <+> text name <+> pPrint repr
      pFields = vcat $ map pPrint fields

instance Pretty EnumSpecValue where
  pPrint (EnumSpecValue name cnst) = parens $ text "value" <+> text name <+> pPrint cnst
