module Data.Cauterize.Parser where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Control.Monad (liftM)

import Data.Cauterize.Types
import Data.Cauterize.Parser.Utils

parseCauterize :: Parser Cauterize
parseCauterize = parens $ do
  string "cauterize" >> spaces'
  n <- parseName
  spaces
  v <- parseVersion
  spaces
  liftM (Cauterize n v) (parseRule `sepBy` many1 space)

typeName :: Parser TypeName
typeName = do
    f <- oneOf first
    rs <- many $ oneOf rest
    return $ T.pack (f:rs)
  where
    under = "_"
    digits = ['0'..'9']
    lowers = ['a'..'z']
    uppers = ['A'..'Z']
    first = under ++ lowers ++ uppers
    rest = first ++ digits

fieldName :: Parser FieldName
fieldName = typeName

parseRule :: Parser CauterizeRule
parseRule = pType
  where
    pType = liftM CauterizeType parseType

litThenQuoted :: String -> (T.Text -> b) -> Parser b
litThenQuoted s c = string s >> spaces' >> liftM c quoted

parseName :: Parser CauterizeName
parseName = liftM CauterizeName quoted

parseVersion :: Parser CauterizeVersion
parseVersion = liftM CauterizeVersion quoted

parseType :: Parser CautType
parseType = try pScalar
        <|> try pEnumeration
        <|> try pFixed
        <|> try pBounded
        <|> try pComposite
        <|>     pGroup
  where
    pScalar = liftM CautScalar parseScalar
    pEnumeration = liftM CautEnumeration parseEnumeration
    pFixed = liftM CautFixed parseFixed
    pBounded = liftM CautBounded parseBounded
    pComposite = liftM CautComposite parseComposite
    pGroup = liftM CautGroup parseGroup

string_ :: String -> Parser ()
string_ n = string n >> spaces'

parseScalar :: Parser Scalar
parseScalar = parens $ do
  string_ "scalar"
  t1 <- typeName 
  spaces
  t2 <- typeName 
  return $ Scalar t1 t2

parseEnumeration :: Parser Enumeration
parseEnumeration = parens $ do
  string_ "enumeration"
  t <- typeName
  spaces
  vs <- parseEnumValue `sepBy1` spaces
  return $ Enumeration t vs

parseEnumValue :: Parser EnumValue
parseEnumValue = parens $ do
  string_ "value"
  t <- typeName
  c <- option Nothing (spaces >> liftM Just parseConst)
  return $ EnumValue t c

parseConst :: Parser Const
parseConst = liftM (DecConst . T.pack) (many1 digit)

parseArray :: String -> (TypeName -> TypeName -> Const -> a) -> Parser a
parseArray n p = parens $ do
  string_ n
  t <- typeName
  spaces
  arrayType <- typeName
  spaces
  l <- parseConst
  return $ p t arrayType l
  

parseFixed :: Parser FixedArray
parseFixed = parseArray "fixed" FixedArray

parseBounded :: Parser BoundedArray
parseBounded = parseArray "bounded" BoundedArray

parseCollection :: String -> (TypeName -> [Field] -> a) -> Parser a
parseCollection n c = parens $ do
  string n >> spaces'
  t <- typeName
  spaces
  fs <- parseField `sepBy1` spaces
  return $ c t fs

parseField :: Parser Field
parseField = parens $ do
  string "field" >> spaces'
  f <- fieldName
  spaces
  t <- typeName
  return $ Field f t

parseComposite :: Parser Composite
parseComposite = parseCollection "composite" Composite

parseGroup :: Parser Group
parseGroup = parseCollection "group" Group
