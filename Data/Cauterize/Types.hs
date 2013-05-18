{-# LANGUAGE DeriveDataTypeable #-}
module Data.Cauterize.Types where

import Data.Data
import Data.Text (Text, unpack)
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

type TypeName = Text
type FieldName = Text

-- |The top level 'Cauterize' parser type. The schema parser returns an
-- instance of this type.
data Cauterize = Cauterize [CauterizeRule] -- ^ A schema is a list of rules.
  deriving (Show, Data, Typeable)

data CauterizeRule = CauterizeInfo { unInfo :: CautInfo }
                   | CauterizeType { unType :: CautType }
  deriving (Show, Data, Typeable)

data CautInfo = CautName { unCautName :: Text }
              | CautVersion { unCautVersion :: Text }
  deriving (Show, Data, Typeable)

data CautType = CautScalar Scalar
              | CautEnumeration Enumeration
              | CautFixed FixedArray
              | CautBounded BoundedArray
              | CautComposite Composite
              | CautGroup Group
  deriving (Show, Data, Typeable)

data Scalar = Scalar TypeName TypeName
  deriving (Show, Data, Typeable)

data Enumeration = Enumeration TypeName [EnumValue]
  deriving (Show, Data, Typeable)

data EnumValue = EnumValue TypeName (Maybe Const)
  deriving (Show, Data, Typeable)

data Const = HexConst Text
           | DecConst Text
           | OctConst Text
           | BinConst Text
  deriving (Show, Data, Typeable)

data FixedArray = FixedArray TypeName TypeName Const
  deriving (Show, Data, Typeable)

data BoundedArray = BoundedArray TypeName TypeName Const
  deriving (Show, Data, Typeable)

data Composite = Composite TypeName [Field]
  deriving (Show, Data, Typeable)

data Group = Group TypeName [Field]
  deriving (Show, Data, Typeable)

data Field = Field FieldName TypeName
  deriving (Show, Data, Typeable)

{- And now, some Show instances. -}

ppTxt :: Text -> Doc
ppTxt = text . unpack

instance Pretty Cauterize where
  pPrint (Cauterize rules) = parens $ text "cauterize" <+> pRules
    where
      pRules = vcat $ map pPrint rules

instance Pretty CauterizeRule where
  pPrint (CauterizeInfo info) = pPrint info
  pPrint (CauterizeType typ) = pPrint typ

instance Pretty CautInfo where
  pPrint i = parens $ case i of
                        CautName n -> text "name" <+> dqPpTxt n
                        CautVersion v -> text "version" <+> dqPpTxt v
    where
      dqPpTxt = doubleQuotes . ppTxt

instance Pretty CautType where
  pPrint t = parens $ case t of
                        (CautScalar (Scalar n m)) -> text "scalar" <+> ppTxt n <+> ppTxt m
                        (CautEnumeration (Enumeration n vs)) -> text "enumeration" <+> ppTxt n <+> pVM vs
                        (CautFixed (FixedArray n m c)) -> text "fixed" <+> ppTxt n <+> ppTxt m <+> pPrint c
                        (CautBounded (BoundedArray n m c)) -> text "bounded" <+> ppTxt n <+> ppTxt m <+> pPrint c
                        (CautComposite (Composite n fs)) -> text "composite" <+> ppTxt n <+> pVM fs
                        (CautGroup (Group n fs)) -> text "group" <+> ppTxt n <+> pVM fs
    where
      pVM vs = vcat $ map pPrint vs
    

instance Pretty EnumValue where
  pPrint (EnumValue n c)
    = let c' = case c of
                    Nothing -> empty
                    Just i -> pPrint i
      in lparen <> text "value" <+> ppTxt n <+> c' <> rparen

instance Pretty Const where
  pPrint (HexConst t) = ppTxt t
  pPrint (DecConst t) = ppTxt t
  pPrint (OctConst t) = ppTxt t
  pPrint (BinConst t) = ppTxt t

instance Pretty Field where
  pPrint (Field n m) = parens $ text "field" <+> ppTxt n <+> ppTxt m
