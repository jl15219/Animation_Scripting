{-# LANGUAGE StandaloneDeriving #-}

module CsvFileConverter where

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.String

parseArr :: Parser ([([[Double]],[[Double]])],[([[Double]],[[Double]])])
parseArr = (,) <$> pure [] <*> many parseSLS

parseSLS :: Parser ([[Double]],[[Double]])
parseSLS = (,) <$> pure [] <*> many parseCell <* string "\n"

parseCell :: Parser [Double]
parseCell = try (string "\"[" *> many parseNum <* string "]\",")
         <|> try (string "\"[" *> many parseNum <* string "]\"")
         <|> string "," *> pure []

parseNum :: Parser Double
parseNum = try ((some (oneOf (['0' .. '9'] ++ ['-','.'])) >>= return . read) <* string ",")
        <|> (some (oneOf (['0' .. '9'] ++ ['-','.'])) >>= return . read)

parseCSV :: Parser ([([Double],[Double])], [([Double],[Double])])
parseCSV = (,) <$> pure [] <*> many parseCSVSLS where
  parseCSVSLS :: Parser ([Double],[Double])
  parseCSVSLS = (,) <$> pure [] <*> many parseCellNum <* string "\n"
  parseCellNum :: Parser Double
  parseCellNum = try (string "\"" *> parseNum <* string "\",")
           <|> try (string "\"" *> parseNum <* string "\"")
           <|> try (parseNum <* string ",")
           <|> parseNum



parseAny :: String -> Parser a -> Maybe a
parseAny s a = case runParser a "ERROR" s of
                Left err -> Nothing
                Right p -> Just p

parseFromFile :: Show a => Parser a -> FilePath -> IO ()
parseFromFile prog filePath = do
  file <- readFile filePath
  putStrLn $ case parse prog filePath file of
    Left err   -> show err
    Right prog -> show prog

testCSV :: String -> FilePath
testCSV s = ("test/test" ++ s ++ ".csv")

csvFile :: String -> FilePath
csvFile s = s ++ ".csv"
