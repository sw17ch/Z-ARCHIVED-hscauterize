module Data.Cauterize.Parser ( parseSchema ) where

import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text
import Control.Monad (liftM)
import qualified Data.Map as M

import Data.Cauterize.Schema
import Data.Cauterize.Parser.Utils

parseSchema :: Parser Schema
parseSchema = parens $ do
  string "cauterize" >> spaces'
  n <- parseName
  spaces
  v <- parseVersion
  spaces

  rs <- parseRule `sepBy` many1 space
  return $ Schema n v (M.fromList $ namedRules rs)
  where
    namedRules :: [SchemaRule] -> [(TypeName, SchemaRule)]
    namedRules rs = zip (map typeName rs) rs

parseTypeName :: Parser TypeName
parseTypeName = do
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

parseFieldName :: Parser FieldName
parseFieldName = parseTypeName

parseRule :: Parser SchemaRule
parseRule = pType
  where
    pType = liftM SchemaType parseType

parseName :: Parser SchemaName
parseName = liftM SchemaName quoted

parseVersion :: Parser SchemaVersion
parseVersion = liftM SchemaVersion quoted

parseType :: Parser SchemaType
parseType = try parseScalar
        <|> try parseEnumeration
        <|> try parseFixed
        <|> try parseBounded
        <|> try parseComposite
        <|>     parseGroup

string_ :: String -> Parser ()
string_ n = string n >> spaces'

parseScalar :: Parser SchemaType
parseScalar = parens $ do
  string_ "scalar"
  t1 <- parseTypeName 
  spaces
  t2 <- parseTypeName 
  return $ SchemaScalar t1 t2

parseEnumeration :: Parser SchemaType
parseEnumeration = parens $ do
  string_ "enumeration"
  t <- parseTypeName
  spaces
  vs <- parseEnumValue `sepBy1` spaces
  return $ SchemaEnumeration t vs

parseEnumValue :: Parser EnumValue
parseEnumValue = parens $ do
  string_ "value"
  t <- parseTypeName
  c <- option Nothing (spaces >> liftM Just parseConst)
  return $ EnumValue t c

parseConst :: Parser Const
parseConst = do
  ds <- many1 digit
  return $ DecConst (read ds) (T.pack ds)

parseArray :: String -> (TypeName -> TypeName -> Const -> a) -> Parser a
parseArray n p = parens $ do
  string_ n
  t <- parseTypeName
  spaces
  arrayType <- parseTypeName
  spaces
  l <- parseConst
  return $ p t arrayType l
  

parseFixed :: Parser SchemaType
parseFixed = parseArray "fixed" SchemaFixedArray

parseBounded :: Parser SchemaType
parseBounded = parseArray "bounded" SchemaBoundedArray

parseCollection :: String -> (TypeName -> [Field] -> a) -> Parser a
parseCollection n c = parens $ do
  string n >> spaces'
  t <- parseTypeName
  spaces
  fs <- parseField `sepBy1` spaces
  return $ c t fs

parseField :: Parser Field
parseField = parens $ do
  string "field" >> spaces'
  f <- parseFieldName
  spaces
  t <- parseTypeName
  return $ Field f t

parseComposite :: Parser SchemaType
parseComposite = parseCollection "composite" SchemaComposite

parseGroup :: Parser SchemaType
parseGroup = parseCollection "group" SchemaGroup
