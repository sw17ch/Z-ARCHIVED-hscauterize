{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Cauterize.Specification
  ( Spec(..)
  , SpecName(..)
  , SpecVersion(..)
  , SpecHash(..)
  , fromSchema
  ) where

import Data.Cauterize.Schema
import Data.Data
import Data.String
import qualified Data.Text as T
import qualified Data.Map as M

fromSchema :: Schema -> Spec
fromSchema (Schema (SchemaName n) (SchemaVersion v) ts) =
  let s = Spec
            (SpecName n)
            (SpecVersion v)
            (SpecHash $ mkSpecHash s)
            (fromTypes ts)
  in s

mkSpecHash :: Spec -> T.Text
mkSpecHash _ = "INVALID"

fromTypes :: M.Map TypeName SchemaType -> M.Map TypeName SpecType
fromTypes src = undefined

fromRule :: SchemaType -> SpecType
fromRule = undefined

data Spec = Spec
  { specName :: SpecName
  , specVersion :: SpecVersion
  , specHash :: SpecHash
  , specTypes :: M.Map TypeName SpecType
  }

newtype SpecName = SpecName T.Text
  deriving (Show, Data, Typeable, IsString)

newtype SpecVersion = SpecVersion T.Text
  deriving (Show, Data, Typeable, IsString)

newtype SpecHash = SpecHash T.Text
  deriving (Show, Data, Typeable, IsString)

data SpecType = SpecScalar TypeName TypeName
              | SpecEnumeration TypeName [EnumValue] BuiltIn
              | SpecFixed TypeName TypeName Const
              | SpecBounded TypeName TypeName Const BuiltIn
              | SpecComposite TypeName [Field]
              | SpecGroup TypeName [Field] BuiltIn
  deriving (Show, Data, Typeable)
