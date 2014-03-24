{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Cauterize.Types ( Cauterize(..)
                            , CauterizeName(..)
                            , CauterizeVersion(..)
                            , CauterizeRule(..)
                            , CautType(..)

                            , Scalar(..)
                            , Enumeration(..)
                            , FixedArray(..)
                            , BoundedArray(..)
                            , Composite(..)
                            , Group(..)

                            , Field(..)
                            , EnumValue(..)

                            , Const(..)

                            , TypeName
                            , FieldName

                            , cauterizeName
                            ) where

import Data.Data
import Data.Text (Text, unpack)
import qualified Data.Map as M
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import Data.String

type TypeName = Text
type FieldName = Text

-- |The top level 'Cauterize' parser type. The schema parser returns an
-- instance of this type.
data Cauterize = Cauterize { cauterizeSchemaName :: CauterizeName
                           , cauterizeVersion    :: CauterizeVersion
                           , cauterizeRules      :: M.Map TypeName CauterizeRule
                           } 
  deriving (Show, Data, Typeable)

newtype CauterizeName = CauterizeName { unName :: Text }
  deriving (Show, Data, Typeable, IsString)

newtype CauterizeVersion = CauterizeVersion { unVersion :: Text }
  deriving (Show, Data, Typeable, IsString)

data CauterizeRule = CauterizeType { unType :: CautType }
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

data Const = HexConst { constVal :: Integer, constText :: Text }
           | DecConst { constVal :: Integer, constText :: Text }
           | OctConst { constVal :: Integer, constText :: Text }
           | BinConst { constVal :: Integer, constText :: Text }
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

instance Pretty Cauterize where
  pPrint (Cauterize name version rules) = parens $ text "cauterize" <+> pPrint name <+> pPrint version <+> pRules
    where
      pRules = vcat $ map pPrint (M.elems rules)

instance Pretty CauterizeRule where
  pPrint (CauterizeType typ) = pPrint typ

instance Pretty CauterizeName where
  pPrint n = parens $ dqPpTxt (unName n)

instance Pretty CauterizeVersion where
  pPrint n = parens $ dqPpTxt (unVersion n)

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
  pPrint (HexConst _ t) = ppTxt t
  pPrint (DecConst _ t) = ppTxt t
  pPrint (OctConst _ t) = ppTxt t
  pPrint (BinConst _ t) = ppTxt t

instance Pretty Field where
  pPrint (Field n m) = parens $ text "field" <+> ppTxt n <+> ppTxt m

class CauterizeNamed a where
  cauterizeName :: a -> TypeName

instance CauterizeNamed CauterizeRule where
  cauterizeName (CauterizeType (CautScalar (Scalar n _))) = n
  cauterizeName (CauterizeType (CautEnumeration (Enumeration n _))) = n
  cauterizeName (CauterizeType (CautFixed (FixedArray n _ _))) = n
  cauterizeName (CauterizeType (CautBounded (BoundedArray n _ _))) = n
  cauterizeName (CauterizeType (CautComposite (Composite n _))) = n
  cauterizeName (CauterizeType (CautGroup (Group n _))) = n

{- And now, some helper functions. -}

ppTxt :: Text -> Doc
ppTxt = text . unpack

dqPpTxt :: Text -> Doc
dqPpTxt = doubleQuotes . ppTxt
