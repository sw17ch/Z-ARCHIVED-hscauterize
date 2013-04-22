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
  liftM Cauterize (parseRule `sepBy` many1 space)

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

parseRule :: Parser CauterizeRule
parseRule = try pInfo <|> pType
  where
    pInfo = liftM CauterizeInfo parseInfo
    pType = liftM CauterizeType parseType

parseInfo :: Parser CautInfo
parseInfo = parens $ parseName <|> parseVersion
  where
    parseName = litThenQuoted "name" CautName
    parseVersion = litThenQuoted "version" CautVersion
    litThenQuoted s c = string s >> spaces' >> liftM c quoted

parseType :: Parser CautType
parseType = try pScalar <|> pEnumeration
  where
    pScalar = liftM CautScalar parseScalar
    pEnumeration = liftM CautEnumeration parseEnumeration

parseScalar :: Parser Scalar
parseScalar = parens $ do
  string "scalar" >> spaces'
  t1 <- typeName 
  spaces
  t2 <- typeName 
  return $ Scalar t1 t2

parseEnumeration :: Parser Enumeration
parseEnumeration = parens $ do
  string "enumeration" >> spaces'
  t <- typeName
  spaces
  vs <- parseEnumValue `sepBy1` spaces
  return $ Enumeration t vs

parseEnumValue :: Parser EnumValue
parseEnumValue = parens $ do
  string "value" >> spaces'
  t <- typeName
  c <- option Nothing (spaces >> liftM Just parseEnumConst)
  return $ EnumValue t c

parseEnumConst :: Parser EnumConst
parseEnumConst = liftM (DecConst . T.pack) (many1 digit)
