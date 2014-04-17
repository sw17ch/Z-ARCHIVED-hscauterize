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
fromSchema (Schema (SchemaName n) (SchemaVersion v) rs) =
  let s = Spec
            (SpecName n)
            (SpecVersion v)
            (SpecHash $ mkSpecHash s)
            (fromRules rs)
  in s

mkSpecHash :: Spec -> T.Text
mkSpecHash _ = "INVALID"

fromRules :: M.Map TypeName SchemaRule -> M.Map TypeName SpecRule
fromRules src = undefined

fromRule :: SchemaRule -> SpecRule
fromRule = undefined

data Spec = Spec
  { specName :: SpecName
  , specVersion :: SpecVersion
  , specHash :: SpecHash
  , specRules :: M.Map TypeName SpecRule
  }

newtype SpecName = SpecName T.Text
  deriving (Show, Data, Typeable, IsString)

newtype SpecVersion = SpecVersion T.Text
  deriving (Show, Data, Typeable, IsString)

newtype SpecHash = SpecHash T.Text
  deriving (Show, Data, Typeable, IsString)

data SpecRule = SpecScalar TypeName TypeName
              | SpecEnumeration TypeName [EnumValue] BuiltIn
              | SpecFixed TypeName TypeName Const
              | SpecBounded TypeName TypeName Const BuiltIn
              | SpecComposite TypeName [Field]
              | SpecGroup TypeName [Field] BuiltIn
  deriving (Show, Data, Typeable)
