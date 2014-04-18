{-# LANGUAGE DeriveDataTypeable #-}

module Data.Cauterize.Types.Field
  ( Field(..)
  , FieldName
  ) where

import Data.Data
import Data.Cauterize.Types.TypeName
import Text.PrettyPrint.HughesPJClass

type FieldName = String

data Field = Field FieldName TypeName
  deriving (Show, Data, Typeable)

instance Pretty Field where
  pPrint (Field n m) = parens $ text "field" <+> text n <+> text m
