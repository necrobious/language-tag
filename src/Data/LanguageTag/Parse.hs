{-# LANGUAGE OverloadedStrings #-}
module Data.LanguageTag.Parse where

import Data.Char (isAlpha)
import Data.LanguageTag.Types
import Data.Attoparsec.Text
import Control.Applicative
import qualified Data.ISO639.Parse  as ISO639 
import qualified Data.ISO3166.Parse as ISO3166 
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Textual
  
parseLanguageTag :: Parser LanguageTag
parseLanguageTag = do
  primary   <- parsePrimaryTag
  secondary <- option Nothing ((char '-') >> (Just <$> parseFirstSubTag))
  subtags   <- many ((char '-') >> (parseSubTag)) 
  return $ LanguageTag primary $ maybe Nothing (\ f -> Just  (f subtags)) secondary
    
parsePrimaryTag :: Parser PrimaryTag
parsePrimaryTag = (parseIANADefinedTag <|> parsePrivateTag <|> parseISO639Tag) <?> "PrimaryTag" 
  
parseFirstSubTag :: Parser ([SubTag] -> FirstSubTag)
parseFirstSubTag = parseISO3166Alpha2Tag <|> parseIANARegisteredTag

parseSubTag :: Parser SubTag
parseSubTag = SubTag <$> (range 3 8 isAlpha)

parseISO639Tag :: Parser PrimaryTag
parseISO639Tag = do
 sixThreeNine <- (count 2 letter) <?> "ISO639 language code parser"
 case ISO639.parseAlpha2 (toText sixThreeNine) of
   Nothing     -> fail (sixThreeNine ++ " is not a valid ISO639 language code!")
   Just iso639 -> return (ISO639Tag iso639)

parseIANADefinedTag :: Parser PrimaryTag
parseIANADefinedTag = ((char 'i') <?> "IANA Defined Tag") >> return IANADefinedTag 
 
parsePrivateTag :: Parser PrimaryTag
parsePrivateTag = ((char 'x') <?> "Private Tag") >> return PrivateTag

parseISO3166Alpha2Tag :: Parser ([SubTag] -> FirstSubTag) 
parseISO3166Alpha2Tag = do 
  threeOneSixSix <- (count 2 letter) <?> "ISO3166 country code parser"
  case ISO3166.parseAlpha2 (toText threeOneSixSix) of
    Nothing -> fail (threeOneSixSix ++ " is not a valid ISO3166 country code!")
    Just iso3166 -> return (ISO3166Alpha2Tag iso3166)

parseIANARegisteredTag :: Parser ([SubTag] -> FirstSubTag)
parseIANARegisteredTag = IANARegisteredTag <$> (range 3 8 isAlpha) 
  

range :: Int -> Int -> (Char -> Bool) -> Parser Text
range min max predicate = do
  result <- (scan 0 doScan) <?> ("text range between "++(show min)++" and "++(show max)++".")
  let len = Text.length result 
  if len < min 
    then fail ("Not enough characters in range, expected: "++(show min)++", actual: "++(show len)++".") 
    else if len > max
      then fail ("Too many characters in range, expected: "++(show max)++", actual: "++(show len)++".") 
      else return result
  where
  doScan count char = 
    if count > max 
      then Nothing 
      else if predicate char then Just (count + 1) else Nothing
 
