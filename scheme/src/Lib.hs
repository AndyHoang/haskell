
module Lib
    ( someFunc
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

import qualified Text.Parsec as Parsec
data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
    deriving (Show)


someFunc :: IO ()
someFunc = do
  args <- getArgs
  putStrLn (show $ readExpr $ args !! 0)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"


readExpr :: String -> String
readExpr input = case parse parseExpr "Lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value "++ show val

spaces :: Parser ()
spaces = skipMany1 space

escapedChar :: Parser Char
escapedChar = char '\\' >> oneOf("\"nrt\\") >>= \c ->
                            return $ case c of
                                    '\\' -> '\\'
                                    'n' -> '\n'
                                    'r' -> '\r'
                                    't' -> '\t'
                                    '"' -> '"'

parseString :: Parser LispVal
parseString = do  char '"'
                  x <- many (escapedChar <|> noneOf ['\\', '"'] )
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


