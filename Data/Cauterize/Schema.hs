{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Cauterize.Schema
  ( Schema(..)
  , SchemaName(..)
  , SchemaVersion(..)
  , SchemaRule(..)
  , SchemaType(..)

  , Field(..)
  , EnumValue(..)

  , Const(..)

  , TypeName
  , FieldName

  , BuiltIn

  , typeName
  ) where

import Data.Data
import Data.Text (Text, unpack)
import qualified Data.Map as M
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import Data.String

type TypeName = Text
type FieldName = Text

-- |The top level 'Schema' parser type. The schema parser returns an
-- instance of this type.
data Schema = Schema { schemaName    :: SchemaName
                     , schemaVersion :: SchemaVersion
                     , schemaRules   :: M.Map TypeName SchemaRule
                     } 
  deriving (Show, Data, Typeable)

newtype SchemaName = SchemaName { unName :: Text }
  deriving (Show, Data, Typeable, IsString)

newtype SchemaVersion = SchemaVersion { unVersion :: Text }
  deriving (Show, Data, Typeable, IsString)

data SchemaRule = SchemaType { unType :: SchemaType }
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

data Const = HexConst { constVal :: Integer, constText :: Text }
           | DecConst { constVal :: Integer, constText :: Text }
           | OctConst { constVal :: Integer, constText :: Text }
           | BinConst { constVal :: Integer, constText :: Text }
  deriving (Show, Data, Typeable)

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
  pPrint (Schema name version rules) = parens $ text "schema" <+> pPrint name <+> pPrint version <+> pRules
    where
      pRules = vcat $ map pPrint (M.elems rules)

instance Pretty SchemaRule where
  pPrint (SchemaType typ) = pPrint typ

instance Pretty SchemaName where
  pPrint n = parens $ dqPpTxt (unName n)

instance Pretty SchemaVersion where
  pPrint n = parens $ dqPpTxt (unVersion n)

instance Pretty SchemaType where
  pPrint t = parens $ case t of
                        (SchemaScalar n m) -> text "scalar" <+> ppTxt n <+> ppTxt m
                        (SchemaEnumeration n vs) -> text "enumeration" <+> ppTxt n <+> pVM vs
                        (SchemaFixedArray n m c) -> text "fixed" <+> ppTxt n <+> ppTxt m <+> pPrint c
                        (SchemaBoundedArray n m c) -> text "bounded" <+> ppTxt n <+> ppTxt m <+> pPrint c
                        (SchemaComposite n fs) -> text "composite" <+> ppTxt n <+> pVM fs
                        (SchemaGroup n fs) -> text "group" <+> ppTxt n <+> pVM fs
    where
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

instance TypeNamed SchemaRule where
  typeName (SchemaType (SchemaScalar n _)) = n
  typeName (SchemaType (SchemaEnumeration n _)) = n
  typeName (SchemaType (SchemaFixedArray n _ _)) = n
  typeName (SchemaType (SchemaBoundedArray n _ _)) = n
  typeName (SchemaType (SchemaComposite n _)) = n
  typeName (SchemaType (SchemaGroup n _)) = n

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

{- And now, some helper functions. -}

ppTxt :: Text -> Doc
ppTxt = text . unpack

dqPpTxt :: Text -> Doc
dqPpTxt = doubleQuotes . ppTxt
