{-# LANGUAGE DeriveDataTypeable #-}

module Data.Cauterize.Types.Field
  ( Field(..)
  , FieldName
  , arbitraryName
  ) where

import Data.Data
import Data.Cauterize.Types.TypeName
import Text.PrettyPrint.HughesPJClass

import Control.Monad

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

type FieldName = String

data Field = Field FieldName TypeName
  deriving (Show, Data, Typeable)

instance Pretty Field where
  pPrint (Field n m) = parens $ text "field" <+> text n <+> text m

instance Arbitrary Field where
  arbitrary = liftM2 Field arbitraryName arbitraryName

arbitraryName :: Gen String
arbitraryName = listOf1 (elements ['a'..'z'])
