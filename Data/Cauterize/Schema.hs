{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Cauterize.Schema
  ( Schema(..)
  , SchemaName(..)
  , SchemaVersion(..)
  , SchemaRule(..)
  , SchemaType(..)

  , Scalar(..)
  , Enumeration(..)
  , FixedArray(..)
  , BoundedArray(..)
  , Composite(..)
  , Group(..)

  , Field(..)
  , EnumValue(..)

  , Const(..)

  , TypeName
  , FieldName

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

data SchemaType = SchemaScalar Scalar
                | SchemaEnumeration Enumeration
                | SchemaFixed FixedArray
                | SchemaBounded BoundedArray
                | SchemaComposite Composite
                | SchemaGroup Group
  deriving (Show, Data, Typeable)

data Scalar = Scalar TypeName TypeName
  deriving (Show, Data, Typeable)

data Enumeration = Enumeration TypeName [EnumValue]
  deriving (Show, Data, Typeable)

data EnumValue = EnumValue TypeName (Maybe Const)
  deriving (Show, Data, Typeable)

data Const = HexConst { constVal :: Integer, constText :: Text }
           | DecConst { constVal :: Integer, constText :: Text }
           | OctConst { constVal :: Integer, constText :: Text }
           | BinConst { constVal :: Integer, constText :: Text }
  deriving (Show, Data, Typeable)

data FixedArray = FixedArray TypeName TypeName Const
  deriving (Show, Data, Typeable)

data BoundedArray = BoundedArray TypeName TypeName Const
  deriving (Show, Data, Typeable)

data Composite = Composite TypeName [Field]
  deriving (Show, Data, Typeable)

data Group = Group TypeName [Field]
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
                        (SchemaScalar (Scalar n m)) -> text "scalar" <+> ppTxt n <+> ppTxt m
                        (SchemaEnumeration (Enumeration n vs)) -> text "enumeration" <+> ppTxt n <+> pVM vs
                        (SchemaFixed (FixedArray n m c)) -> text "fixed" <+> ppTxt n <+> ppTxt m <+> pPrint c
                        (SchemaBounded (BoundedArray n m c)) -> text "bounded" <+> ppTxt n <+> ppTxt m <+> pPrint c
                        (SchemaComposite (Composite n fs)) -> text "composite" <+> ppTxt n <+> pVM fs
                        (SchemaGroup (Group n fs)) -> text "group" <+> ppTxt n <+> pVM fs
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
  typeName (SchemaType (SchemaScalar (Scalar n _))) = n
  typeName (SchemaType (SchemaEnumeration (Enumeration n _))) = n
  typeName (SchemaType (SchemaFixed (FixedArray n _ _))) = n
  typeName (SchemaType (SchemaBounded (BoundedArray n _ _))) = n
  typeName (SchemaType (SchemaComposite (Composite n _))) = n
  typeName (SchemaType (SchemaGroup (Group n _))) = n

{- And now, some helper functions. -}

ppTxt :: Text -> Doc
ppTxt = text . unpack

dqPpTxt :: Text -> Doc
dqPpTxt = doubleQuotes . ppTxt
