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
              | CautFixed FixedArray
              | CautBounded BoundedArray
              | CautComposite Composite
              | CautGroup Group
  deriving (Show)

data Scalar = Scalar TypeName TypeName
  deriving (Show)

data Enumeration = Enumeration TypeName [EnumValue]
  deriving (Show)

data EnumValue = EnumValue TypeName (Maybe Const)
  deriving (Show)

data Const = HexConst Text
           | DecConst Text
           | OctConst Text
           | BinConst Text
  deriving (Show)

data FixedArray = FixedArray TypeName TypeName Const
  deriving (Show)

data BoundedArray = BoundedArray TypeName TypeName Const
  deriving (Show)

data Composite = Composite TypeName [Field]
  deriving (Show)

data Group = Group TypeName [Field]
  deriving (Show)

data Field = Field FieldName TypeName
  deriving (Show)
