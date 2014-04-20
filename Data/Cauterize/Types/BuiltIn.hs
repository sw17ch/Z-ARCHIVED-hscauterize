{-# LANGUAGE DeriveDataTypeable #-}

module Data.Cauterize.Types.BuiltIn
  ( BuiltIn(..)
  ) where

import Data.Data
import Data.Cauterize.Types.TypeName
import Text.PrettyPrint.HughesPJClass

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

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
  deriving (Show, Data, Typeable, Enum, Bounded)

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
  pPrint b = text $ typeName b

instance Arbitrary BuiltIn where
  arbitrary = elements [minBound ..]
