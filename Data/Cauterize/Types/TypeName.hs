module Data.Cauterize.Types.TypeName
  ( TypeName
  , TypeNamed(..)
  ) where

type TypeName = String

class TypeNamed a where
  typeName :: a -> TypeName

