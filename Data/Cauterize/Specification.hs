{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Cauterize.Specification
  ( Spec(..)
  , SpecName
  , SpecVersion
  , SpecHash
  , fromSchema
  ) where

import Data.Cauterize.Schema
import Data.Data
import qualified Data.Text as T
import qualified Data.Map as M

type SpecName = T.Text
type SpecVersion = T.Text
type SpecHash = T.Text

fromSchema :: Schema -> Spec
fromSchema (Schema n v ts) =
  let s = Spec n v (mkSpecHash s) (fromTypes ts) in s

mkSpecHash :: Spec -> T.Text
mkSpecHash _ = "INVALID"

fromTypes :: M.Map TypeName SchemaType -> M.Map TypeName SpecType
fromTypes = M.map fromType

fromType :: SchemaType -> SpecType
-- Scalars have no meta-data
fromType (SchemaScalar name target) = SpecScalar name target
-- Enumerations need to assign constants to each member at this stage
-- TODO: This is incomplete
fromType (SchemaEnumeration name values) = SpecEnumeration name [] BiUint64
-- Fixed arrays have no meta data
fromType (SchemaFixedArray name target cnst) = SpecFixed name target cnst
-- Bounded arrays need to determine the bits needed to store the length
fromType (SchemaBoundedArray name target cnst) = SpecBounded name target cnst BiUint64
-- Composites have no meta-data
fromType (SchemaComposite name fields) = SpecComposite name fields
-- Groups need to determine how many bits are needed to represent the type tag
fromType (SchemaGroup name fields) = SpecGroup name fields BiUint64

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
