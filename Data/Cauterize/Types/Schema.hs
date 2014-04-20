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

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Control.Monad

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
      pSchema = hsep [text "schema", text name, text version]
      pRules = vcat $ map pPrint (M.elems rules)

instance Pretty SchemaType where
  pPrint (SchemaScalar n m) = parens $ hsep [text "scalar", text n, text m]
  pPrint (SchemaEnumeration n vs) = parens $ hang pE 1 (pVM vs)
    where
      pE = text "enumeration" <+> text n
  pPrint (SchemaFixedArray n m c) = parens $ hsep [text "fixed", text n, text m, pPrint c]
  pPrint (SchemaBoundedArray n m c) = parens $ hsep [text "bounded", text n, text m, pPrint c]
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
      in lparen <> hsep [text "value", text n, c'] <> rparen

instance Pretty Const where
  pPrint (Const v) = text $ show v

instance TypeNamed SchemaType where
  typeName (SchemaScalar n _) = n
  typeName (SchemaEnumeration n _) = n
  typeName (SchemaFixedArray n _ _) = n
  typeName (SchemaBoundedArray n _ _) = n
  typeName (SchemaComposite n _) = n
  typeName (SchemaGroup n _) = n

instance Arbitrary Schema where
  arbitrary = do
    n <- arbitraryName
    t <- arbitraryName
    ts <- listOf1 arbitrary :: Gen [SchemaType]
    let tMap = M.fromList $ map (\st -> (typeName st, st)) ts :: M.Map TypeName SchemaType
    return $ Schema n t tMap

instance Arbitrary SchemaType where
  arbitrary = oneof [ arbScalar, arbEnum, arbFixed, arbBounded, arbComposite, arbGroup ]
    where
      arbScalar = liftM2 SchemaScalar arbitraryName arbitraryName
      arbEnum = liftM2 SchemaEnumeration arbitraryName (listOf1 arbitrary)
      arbFixed = liftM3 SchemaFixedArray arbitraryName arbitraryName arbitrary
      arbBounded = liftM3 SchemaBoundedArray arbitraryName arbitraryName arbitrary
      arbComposite = liftM2 SchemaComposite arbitraryName (listOf1 arbitrary)
      arbGroup = liftM2 SchemaGroup arbitraryName (listOf1 arbitrary)

instance Arbitrary EnumValue where
  arbitrary = liftM2 EnumValue arbitraryName arbitrary

instance Arbitrary Const where
  arbitrary = liftM Const arbitrary
