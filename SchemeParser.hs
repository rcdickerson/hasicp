module SchemeParser (SchemeVal(..),
                     parseSexp)
    where

-- Modified from Jonathan Tang's lispparser package:
-- http://hackage.haskell.org/package/lispparser/

import Text.ParserCombinators.Parsec

data SchemeVal = SchemeAtom String
               | SchemeList [SchemeVal]
               | SchemeNumber Double
               | SchemeBool Bool
               | SchemeString String
  deriving Show

parseSexp :: String -> SchemeVal
parseSexp s = let 
    result = parse expParser [] s
  in case result of
       (Right vals) -> vals

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

skipSpaces :: Parser ()
skipSpaces = skipMany1 space

parseAtom :: Parser SchemeVal
parseAtom = do 
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = [first] ++ rest
  return $ case atom of
             "true" -> SchemeBool True
             "false" -> SchemeBool False
             otherwise -> SchemeAtom atom

parseNumber :: Parser SchemeVal
parseNumber = fmap (SchemeNumber . read) $ many1 digit

parseString :: Parser SchemeVal
parseString = do
  char '"'
  str <- many1 (noneOf "\"")
  char '"'
  return $ SchemeString str

parseList :: Parser SchemeVal
parseList = fmap SchemeList $ sepBy expParser skipSpaces

expParser :: Parser SchemeVal
expParser = parseAtom
        <|> parseNumber
        <|> parseString
        <|> do char '('
               list <- (try parseList)
               char ')'
               return list