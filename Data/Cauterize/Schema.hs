{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Cauterize.Schema
  ( Schema(..)
  , SchemaType(..)
  , BuiltIn(..)

  , Field(..)
  , EnumValue(..)

  , Const(..)

  , TypeName
  , FieldName
  , SchemaName
  , SchemaVersion

  , reinterpConst
  , typeName
  ) where

import Data.Data
import Data.Maybe
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Map as M
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Numeric

type TypeName = T.Text
type FieldName = T.Text
type SchemaName = T.Text
type SchemaVersion = T.Text

-- |The top level 'Schema' parser type. The schema parser returns an
-- instance of this type.
data Schema = Schema { schemaName    :: SchemaName
                     , schemaVersion :: SchemaVersion
                     , schemaRules   :: M.Map TypeName SchemaType
                     } 
  deriving (Show, Data, Typeable)

data SchemaType = SchemaScalar TypeName TypeName
                | SchemaEnumeration TypeName [EnumValue]
                | SchemaFixedArray TypeName TypeName Const
                | SchemaBoundedArray TypeName TypeName Const
                | SchemaComposite TypeName [Field]
                | SchemaGroup TypeName [Field]
  deriving (Show, Data, Typeable)

data EnumValue = EnumValue TypeName (Maybe Const)
  deriving (Show, Data, Typeable)

data Const = HexConst { constVal :: Integer, constText :: T.Text }
           | DecConst { constVal :: Integer, constText :: T.Text }
           | OctConst { constVal :: Integer, constText :: T.Text }
           | BinConst { constVal :: Integer, constText :: T.Text }
  deriving (Show, Data, Typeable)

reinterpConst :: Const -> Integer -> Const
reinterpConst c i = case c of
                      (HexConst _ _) -> HexConst i (T.pack $ showIntAtBase 16 intToChar i "")
                      (DecConst _ _) -> DecConst i (T.pack $ showIntAtBase 10 intToChar i "")
                      (OctConst _ _) -> OctConst i (T.pack $ showIntAtBase  8 intToChar i "")
                      (BinConst _ _) -> BinConst i (T.pack $ showIntAtBase  2 intToChar i "")
  where
    mapping :: [(Int, Char)]
    mapping = zip [0..15] "0123456789ABCDEF"

    intToChar :: Int -> Char
    intToChar = fromJust . flip lookup mapping


data BuiltIn = BiUint8
             | BiUint16
             | BiUint32
             | BiUint64
             | BiInt8
             | BiInt16
             | BiInt32
             | BiInt64
             | BiFloat32
             | BiFloat64
             | BiBool
  deriving (Show, Data, Typeable)


data Field = Field FieldName TypeName
  deriving (Show, Data, Typeable)

{- And now, some Show instances. -}

instance Pretty Schema where
  pPrint (Schema name version rules) = parens $ hang pSchema 1 pRules
    where
      pSchema = text "schema" <+> ppTxt name <+> ppTxt version
      pRules = vcat $ map pPrint (M.elems rules)

instance Pretty SchemaType where
  pPrint (SchemaScalar n m) = parens $ text "scalar" <+> ppTxt n <+> ppTxt m
  pPrint (SchemaEnumeration n vs) = parens $ hang pE 1 (pVM vs)
    where
      pE = text "enumeration" <+> ppTxt n
  pPrint (SchemaFixedArray n m c) = parens $ text "fixed" <+> ppTxt n <+> ppTxt m <+> pPrint c
  pPrint (SchemaBoundedArray n m c) = parens $ text "bounded" <+> ppTxt n <+> ppTxt m <+> pPrint c
  pPrint (SchemaComposite n fs) = parens $ hang pC 1 (pVM fs)
    where
      pC =  text "composite" <+> ppTxt n
  pPrint (SchemaGroup n fs) = parens $ hang pG 1 (pVM fs)
    where
      pG =  text "composite" <+> ppTxt n
     
pVM :: Pretty a => [a] -> Doc
pVM vs = vcat $ map pPrint vs

instance Pretty EnumValue where
  pPrint (EnumValue n c)
    = let c' = case c of
                    Nothing -> empty
                    Just i -> pPrint i
      in lparen <> text "value" <+> ppTxt n <+> c' <> rparen

instance Pretty Const where
  pPrint (HexConst _ t) = ppTxt t
  pPrint (DecConst _ t) = ppTxt t
  pPrint (OctConst _ t) = ppTxt t
  pPrint (BinConst _ t) = ppTxt t

instance Pretty Field where
  pPrint (Field n m) = parens $ text "field" <+> ppTxt n <+> ppTxt m

class TypeNamed a where
  typeName :: a -> TypeName

instance TypeNamed SchemaType where
  typeName (SchemaScalar n _) = n
  typeName (SchemaEnumeration n _) = n
  typeName (SchemaFixedArray n _ _) = n
  typeName (SchemaBoundedArray n _ _) = n
  typeName (SchemaComposite n _) = n
  typeName (SchemaGroup n _) = n

instance TypeNamed BuiltIn where
  typeName BiUint8 = "uint8"
  typeName BiUint16 = "uint16"
  typeName BiUint32 = "uint32"
  typeName BiUint64 = "uint64"
  typeName BiInt8 = "int8"
  typeName BiInt16 = "int16"
  typeName BiInt32 = "int32"
  typeName BiInt64 = "int64"
  typeName BiFloat32 = "float32"
  typeName BiFloat64 = "float64"
  typeName BiBool = "bool"

instance Pretty BuiltIn where
  pPrint BiUint8 = text "uint8"
  pPrint BiUint16 = text "uint16"
  pPrint BiUint32 = text "uint32"
  pPrint BiUint64 = text "uint64"
  pPrint BiInt8 = text "int8"
  pPrint BiInt16 = text "int16"
  pPrint BiInt32 = text "int32"
  pPrint BiInt64 = text "int64"
  pPrint BiFloat32 = text "float32"
  pPrint BiFloat64 = text "float64"
  pPrint BiBool = text "bool"

{- And now, some helper functions. -}

ppTxt :: T.Text -> Doc
ppTxt = text . T.unpack
