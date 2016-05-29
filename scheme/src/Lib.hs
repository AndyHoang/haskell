
module Lib
    ( someFunc
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool


someFunc :: IO ()
someFunc = do
  (expr: _ ) <- getArgs
  putStrLn (readExpr expr)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


readExpr :: String -> String
readExpr input = case parse parseExpr "Lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value "

spaces :: Parser ()
spaces = skipMany1 space


parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many(noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many(letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "$f" -> Bool False
             _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = fmap (Number. read) $ many1 digit
--parseNumber = do
--  num <- many1 digit
--  return $ Number . read $ num
--parseNumber = (many1 digit) >>= (\x -> return ((Number . read) x))


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber


