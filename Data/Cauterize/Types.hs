module Data.Cauterize.Types where

import Data.Text

type TypeName = Text
type FieldName = Text

-- |The top level 'Cauterize' parser type. The schema parser returns an
-- instance of this type.
data Cauterize = Cauterize [CauterizeRule] -- ^ A schema is a list of rules.
  deriving (Show)

data CauterizeRule = CauterizeInfo CautInfo
                   | CauterizeType CautType
  deriving (Show)

data CautInfo = CautName Text
              | CautVersion Text
  deriving (Show)

data CautType = CautScalar Scalar
              | CautEnumeration Enumeration
  deriving (Show)

data Scalar = Scalar TypeName TypeName
  deriving (Show)

data Enumeration = Enumeration TypeName [EnumValue]
  deriving (Show)

data EnumValue = EnumValue TypeName (Maybe EnumConst)
  deriving (Show)

data EnumConst = HexConst Text
               | DecConst Text
               | OctConst Text
               | BinConst Text
  deriving (Show)

data Fixed = Fixed TypeName TypeName Int
  deriving (Show)

data BoundedArray = BoundedArray TypeName TypeName Int
  deriving (Show)

data Composite = Composite TypeName [CompositeField]
  deriving (Show)

data CompositeField = CompositeField FieldName TypeName
  deriving (Show)

data Group = Group TypeName [GroupField]
  deriving (Show)

data GroupField = GroupField FieldName TypeName
  deriving (Show)
