module Lib
    ( someFunc
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Data.Char

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Char Char
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


escapedChar :: Parser Char
escapedChar = char '\\' >> oneOf("\"nrt\\") >>= \c ->
                            return $ case c of
                                    '\\' -> '\\'
                                    'n' -> '\n'
                                    'r' -> '\r'
                                    't' -> '\t'
                                    '"' -> '"'
                                    _ -> c

parseString :: Parser LispVal
parseString = do  char '"'
                  x <- many (escapedChar <|> noneOf ['\\', '"'] )
                  char '"'
                  return $ String x


parseChar:: Parser LispVal
parseChar = do string "#\\"
               s <- many1 letter
               return $ case (map toLower s) of
                          "space" -> Char ' '
                          "newline" -> Char '\n'
                          [x] -> Char x
                          _ ->  String s

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
parseNumber = parsePlainNumber <|> parseRadixNumber

parseRadixNumber :: Parser LispVal
parseRadixNumber = char '#' >>
    (
      (char 'd' >> parsePlainNumber)
      <|> (char 'b' >> parseBinaryNumber)
      <|> (char 'o' >> parseOctalNumber)
      <|> (char 'x' >> parseHexNumber)
    )

parseBinaryNumber :: Parser LispVal
parseBinaryNumber  = readNumberWithBase "01" 2

parseOctalNumber:: Parser LispVal
parseOctalNumber = readNumberWithBase "012345678" 8

parseHexNumber:: Parser LispVal
parseHexNumber = readNumberWithBase "0123456789abcdefABCDEF" 16

parsePlainNumber :: Parser LispVal
parsePlainNumber = fmap (Number. read) $ many1 digit

readNumberWithBase :: String -> Integer -> Parser LispVal
readNumberWithBase digits base = do
                                d <- many (oneOf digits)
                                return $ Number $ toDecimal base d

toDecimal :: Integer -> String -> Integer
toDecimal base str = foldl1 ((+).(*base)) ( map (toInteger . digitToInt)  str)

--parseNumber = do
--  num <- many1 digit
--  return $ Number . read $ num
--parseNumber = (many1 digit) >>= (\x -> return ((Number . read) x))


parseExpr :: Parser LispVal
parseExpr = try parseNumber
         <|> try parseChar
         <|> parseString
         <|> parseAtom
