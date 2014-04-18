{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Cauterize.Types.Schema
  ( Schema(..)
  , SchemaType(..)

  , EnumValue(..)

  , Const(..)

  , SchemaName
  , SchemaVersion

  , typeName
  ) where

import Data.Data
import qualified Data.Map as M
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import Data.Cauterize.Types.TypeName
import Data.Cauterize.Types.Field

type SchemaName = String
type SchemaVersion = String

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

data Const = Const { constVal :: Integer }
  deriving (Show, Data, Typeable)

{- And now, some Show instances. -}

instance Pretty Schema where
  pPrint (Schema name version rules) = parens $ hang pSchema 1 pRules
    where
      pSchema = text "schema" <+> text name <+> text version
      pRules = vcat $ map pPrint (M.elems rules)

instance Pretty SchemaType where
  pPrint (SchemaScalar n m) = parens $ text "scalar" <+> text n <+> text m
  pPrint (SchemaEnumeration n vs) = parens $ hang pE 1 (pVM vs)
    where
      pE = text "enumeration" <+> text n
  pPrint (SchemaFixedArray n m c) = parens $ text "fixed" <+> text n <+> text m <+> pPrint c
  pPrint (SchemaBoundedArray n m c) = parens $ text "bounded" <+> text n <+> text m <+> pPrint c
  pPrint (SchemaComposite n fs) = parens $ hang pC 1 (pVM fs)
    where
      pC =  text "composite" <+> text n
  pPrint (SchemaGroup n fs) = parens $ hang pG 1 (pVM fs)
    where
      pG =  text "composite" <+> text n
     
pVM :: Pretty a => [a] -> Doc
pVM vs = vcat $ map pPrint vs

instance Pretty EnumValue where
  pPrint (EnumValue n c)
    = let c' = case c of
                    Nothing -> empty
                    Just i -> pPrint i
      in lparen <> text "value" <+> text n <+> c' <> rparen

instance Pretty Const where
  pPrint (Const v) = text $ show v

instance TypeNamed SchemaType where
  typeName (SchemaScalar n _) = n
  typeName (SchemaEnumeration n _) = n
  typeName (SchemaFixedArray n _ _) = n
  typeName (SchemaBoundedArray n _ _) = n
  typeName (SchemaComposite n _) = n
  typeName (SchemaGroup n _) = n
