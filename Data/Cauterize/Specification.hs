{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Cauterize.Specification
  ( Spec(..)
  , fromSchema
  ) where

import Data.Cauterize.Schema
import Data.Data
import Data.String
import Data.Text (Text, unpack)
import qualified Data.Map as M

fromSchema :: Schema -> Spec
fromSchema (Schema (SchemaName n) (SchemaVersion v) rs) =
  Spec
    (SpecName n)
    (SpecVersion v)
    (fromRules rs)

fromRules _ = M.empty

data Spec = Spec
  { specName :: SpecName
  , specVersion :: SpecVersion
  , specRules :: M.Map TypeName SpecRule
  }

newtype SpecName = SpecName Text
  deriving (Show, Data, Typeable, IsString)

newtype SpecVersion = SpecVersion Text
  deriving (Show, Data, Typeable, IsString)

data SpecRule = SpecScalar
              | SpecEnumeratino
              | SpecFixed
              | SpecBounded
              | SpecComposite
              | SpecGroup
  deriving (Show, Data, Typeable)
